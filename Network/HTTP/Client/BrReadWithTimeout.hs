{-# LANGUAGE NumDecimals #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Client.BrReadWithTimeout
-- Copyright   :  (c) Alexey Radkov 2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Http client with timeouts applied in between body read events.
--
-- Note that the response timeout in /http-client/ is applied only when
-- receiving the response headers which is not always satisfactory given
-- that a slow server may send the rest of the response very slowly.
-----------------------------------------------------------------------------


module Network.HTTP.Client.BrReadWithTimeout (
                                              fromResponseTimeout
                                             ,brReadWithTimeout
                                             ,httpLbsBrReadWithTimeout
                                             ) where

import           Network.HTTP.Client hiding (HttpExceptionContent (..))
import qualified Network.HTTP.Client as E (HttpExceptionContent (..))
import qualified Network.HTTP.Client.Internal as I (ResponseTimeout (..)
                                                   ,mResponseTimeout
                                                   )
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Control.Exception
import           System.Timeout

-- | Converts 'ResponseTimeout' of the request into the number of microseconds.
fromResponseTimeout :: Request -> Manager -> Int
fromResponseTimeout req man =
    case responseTimeout req of
        I.ResponseTimeoutDefault ->
            case I.mResponseTimeout man of
                I.ResponseTimeoutDefault -> 30e6
                I.ResponseTimeoutNone -> -1
                I.ResponseTimeoutMicro u -> u
        I.ResponseTimeoutNone -> -1
        I.ResponseTimeoutMicro u -> u

-- | Reads the next chunk of the response body with the specified timeout.
--
-- Note that 'brRead' and 'httpLbs' do not apply any timeouts after reading
-- response headers. This may hang the client if the server implementation is
-- buggy, for instance, when it sends a body of a lesser size than the value of
-- the /Content-Length/ response header. This function solves this problem by
-- applying a timeout passed in the first parameter as a number of microseconds
-- between body read events.
--
-- Throws 'E.ResponseTimeout' if reading of the next chunk of the response body
-- timed out.
brReadWithTimeout :: Int -> Request -> BodyReader -> IO ByteString
brReadWithTimeout tmo req br = do
    x <- timeout tmo br
    case x of
        Nothing -> throwIO $ HttpExceptionRequest
            req { responseTimeout = I.ResponseTimeoutMicro tmo }
                E.ResponseTimeout
        Just bs -> return bs

-- | This is like 'httpLbs' but with a timeout between body read events.
--
-- The value of the timeout is retrieved from the 'ResponseTimeout' of the
-- request.
httpLbsBrReadWithTimeout :: Request -> Manager -> IO (Response L.ByteString)
httpLbsBrReadWithTimeout req man = withResponse req man $ \res -> do
    let tmo = fromResponseTimeout req man
    bss <- brConsume $ brReadWithTimeout tmo req $ responseBody res
    return res { responseBody = L.fromChunks bss }

