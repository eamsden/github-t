{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module GitHub.Monad
  ( MonadGitHub(..)
  , GitHubT()
  , runGitHubT
  , runGitHubTWith
  ) where


import Control.Applicative

import Control.Concurrent (threadDelay)

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except

import Data.Aeson (FromJSON())
import           Data.List (lookup)
import qualified Data.Vector as V

import GitHub.Auth
import GitHub.Data.Definitions
import GitHub.Data.Request
import GitHub.Request

import Network.HTTP.Client hiding (Request)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status
import Network.URI

import Data.ByteString.Lex.Integral
import qualified Data.ByteString.Lazy as LBS

import Data.Time.Clock.POSIX
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)

data Limits
  = Limits
  { limitsTotalRequests :: Int
  , limitsRemainingRequests :: Int
  , limitsResetSecondsSinceEpoch :: Int
  }

-- | A typeclass for monads which can run GitHub requests
class MonadError Error m => MonadGitHub m where
  -- | Run a Request in the monad, respecting rate limiting and retrying on errors
  makeRequest :: FromJSON a => Request rw a -> m a
  --  | Run a Request in the Monad, respecting rate limiting
  makeRequestOnce :: FromJSON a => Request rq a -> m a
  -- | Get the rate limits specified by the last request
  getRateLimit :: m Limits

instance (MonadIO m, MonadCatch m) => MonadGitHub (GitHubT m) where
  makeRequest request =
    retryOnTransientErrors 4 $ runRequestWithRateLimiting request
  makeRequestOnce = runRequestWithRateLimiting
  getRateLimit = GitHubT get

retryOnTransientErrors :: MonadIO m => Int -> GitHubT m a -> GitHubT m a
retryOnTransientErrors retries m | retries <= 0 = m
                                 | otherwise = catchAndRetry m
  where
    catchAndRetry m =
      m `catchError` \e ->
        case e of
          err@(HTTPError (HttpExceptionRequest _ content)) -> do 
            case content of
              StatusCodeException response _ ->
                let
                  resHeaders = responseHeaders response
                  lookupIntHeader header =
                    lookup header resHeaders >>= fmap fst . readDecimal
                in GitHubT $ maybe (return ()) put $
                   Limits
                   <$> lookupIntHeader "X-RateLimit-Limit"
                   <*> lookupIntHeader "X-RateLimit-Remaining"
                   <*> lookupIntHeader "X-RateLimit-Reset"
              _ -> return ()
            GitHubT waitForReset
            retryOnTransientErrors (pred retries) m
          err -> throwError err

    retry = retryOnTransientErrors (pred retries) m

waitForReset :: (MonadState Limits m, MonadIO m) => m ()
waitForReset =
  do
    (Limits _total remaining reset) <- get
    if remaining <= 0
      then wait
      else return ()
    modify (\(Limits total remaining reset) -> (Limits total (pred remaining) reset))
  where
    wait = do
      currentTime <- fmap round $ liftIO $ getPOSIXTime
      (Limits total _remaining reset) <- get
      let waitTime = reset - currentTime + 1
      if waitTime > 0
        then (liftIO $ threadDelay $ waitTime * 10^6) >> (put $ Limits total total reset)
        else return ()

runRequestWithRateLimiting :: FromJSON a => MonadIO m => MonadCatch m => Request rw a -> GitHubT m a
runRequestWithRateLimiting request = do
  (auth, _) <- GitHubT ask
  GitHubT waitForReset
  httpRequest <- liftIO $ makeHttpRequest auth request
  performHttpRequest httpRequest request
  where
    httpLbs' :: MonadIO m => MonadCatch m => HTTP.Request -> GitHubT m (Response LBS.ByteString)
    httpLbs' request' = GitHubT $ do
      (_, manager) <- ask
      res <- liftIO (httpLbs request' manager) `catch` (throwError . HTTPError)
      -- Before returning the response we'll peek at the header and
      -- update the Limit if we can
      maybe (return ()) put
        $ let
            resHeaders = responseHeaders res
            lookupIntHeader header =
              lookup header resHeaders >>= fmap fst . readDecimal
          in Limits
             <$> lookupIntHeader "X-RateLimit-Limit"
             <*> lookupIntHeader "X-RateLimit-Remaining"
             <*> lookupIntHeader "X-RateLimit-Reset"
      return res

    performHttpRequest :: FromJSON a => MonadIO m => MonadCatch m => HTTP.Request -> Request rw a -> GitHubT m a
    performHttpRequest httpReq (SimpleQuery sreq) =
      performHttpRequest' httpReq sreq
    performHttpRequest httpReq (HeaderQuery _ sreq) =
      performHttpRequest' httpReq sreq
    performHttpRequest httpReq (StatusQuery sm _) = do
      res <- httpLbs' httpReq
      parseResponse res
    performHttpRequest httpReq (RedirectQuery _) = do
      res <- httpLbs' httpReq
      parseRedirect (getUri httpReq) res

    performHttpRequest' :: FromJSON a => MonadIO m => MonadCatch m => HTTP.Request -> SimpleRequest rw a -> GitHubT m a
    performHttpRequest' httpReq Query {} = do
      res <- httpLbs' httpReq
      parseResponse res
    performHttpRequest' httpReq (PagedQuery _ _ l) =
      performPagedRequest httpLbs' predicate httpReq
      where
        predicate v = lessFetchCount (V.length v) l
        lessFetchCount _ FetchAll = True
        lessFetchCount i (FetchAtLeast j) = i < fromIntegral j
    performHttpRequest' httpReq (Command m _ _) = do
      res <- httpLbs' httpReq
      case m of
        Delete -> pure ()
        Put' -> pure ()
        _ -> parseResponse res


-- | A monad transformer for working with GitHub.
newtype GitHubT m a
  = GitHubT
  { primRunGitHubT :: ExceptT Error (StateT Limits (ReaderT (Maybe Auth, Manager) m)) a } deriving Functor

-- | Run a GitHub computation
runGitHubT :: Monad m => GitHubT m a -> Manager -> Maybe Auth -> Limits -> m (Either Error a)
runGitHubT m manager auth limits =
  flip runReaderT (auth, manager) $ flip evalStateT limits $ runExceptT $ primRunGitHubT m

-- | Run a GitHub computation, supplying the initial Limits value and authentication first
runGitHubTWith :: Monad m => Manager -> Maybe Auth -> Limits -> GitHubT m a -> m (Either Error a)
runGitHubTWith manager auth limits m = runGitHubT m manager auth limits

instance Monad m => Applicative (GitHubT m) where
  pure = GitHubT . pure
  (GitHubT f) <*> (GitHubT x) = GitHubT $ f <*> x

instance Monad m => Monad (GitHubT m) where
  return = GitHubT . return
  (GitHubT m) >>= f =
    GitHubT $ m >>= primRunGitHubT . f

instance MonadFix m => MonadFix (GitHubT m) where
  mfix f = GitHubT $ mfix $ primRunGitHubT . f

instance MonadIO m => MonadIO (GitHubT m) where
  liftIO = GitHubT . liftIO

instance MonadTrans GitHubT where
  lift = GitHubT . lift . lift . lift

instance MonadReader r m => MonadReader r (GitHubT m) where
  ask = lift ask
  local f (GitHubT m) = GitHubT $ mapExceptT (mapStateT (mapReaderT (local f))) m
  reader = lift . reader

instance MonadState s m => MonadState s (GitHubT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance Monad m => MonadError Error (GitHubT m) where
  throwError = GitHubT . throwError
  catchError m handler = GitHubT $ catchError (primRunGitHubT m) (primRunGitHubT . handler)

instance MonadThrow m => MonadThrow (GitHubT m) where
  throwM e = GitHubT $ throwM e

instance MonadCatch m => MonadCatch (GitHubT m) where
  catch m handler = GitHubT $ catch (primRunGitHubT m) (primRunGitHubT . handler)

parseRedirect :: MonadError Error m => URI -> Response LBS.ByteString -> m URI
parseRedirect originalUri rsp = do
    let status = responseStatus rsp
    when (statusCode status /= 302) $
        throwError $ ParseError $ "invalid status: " <> pack (show status)
    loc <- maybe noLocation return $ lookup "Location" $ responseHeaders rsp
    case parseURIReference $ unpack $ decodeUtf8 loc of
        Nothing -> throwError $ ParseError $
            "location header does not contain a URI: " <> pack (show loc)
        Just uri -> return $ uri `relativeTo` originalUri
  where
    noLocation = throwError $ ParseError "no location header in response"
