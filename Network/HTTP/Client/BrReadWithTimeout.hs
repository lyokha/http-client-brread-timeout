{-# LANGUAGE NumDecimals #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Client.BrReadWithTimeout
-- Copyright   :  (c) Alexey Radkov 2022-2026
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


module Network.HTTP.Client.BrReadWithTimeout (fromResponseTimeout
                                             ,brReadWithTimeout
                                             ,brReadSomeWithTimeout
                                             ,brConsumeWithTimeout
                                             ,httpLbsBrReadWithCustomTimeout
                                             ,httpLbsBrReadWithTimeout
                                             ) where

import           Network.HTTP.Client
import qualified Network.HTTP.Client.Internal
                    as I (ResponseTimeout (..), mResponseTimeout)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Control.Exception
import           System.Timeout

-- | Converts t'ResponseTimeout' of the request into the number of microseconds.
--
-- This function returns the value of the timeout on reading response headers
-- which can be used to apply equal timeouts between body read events in
-- `brReadWithTimeout`, `brReadSomeWithTimeout`, and `brConsumeWithTimeout`.
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
-- Throws v'ResponseTimeout' if reading of the next chunk of the response body
-- timed out.
brReadWithTimeout :: Int -> Request -> BodyReader -> IO ByteString
brReadWithTimeout tmo req br = timeout tmo br
    >>= maybe (throwIO $
                  HttpExceptionRequest
                      req { responseTimeout = I.ResponseTimeoutMicro tmo }
                      ResponseTimeout
              ) return

-- | This is like 'brReadSome' but with a timeout between body read events.
--
-- The value of the timeout is passed in the first parameter as a number of
-- microseconds. A negative value effectively disables the timeout which makes
-- the function behave exactly as 'brReadSome'.
--
-- Implemented as `brReadSome` with `brReadWithTimeout` passed in its first
-- parameter.
--
-- @since 0.2.0.0
brReadSomeWithTimeout :: Int -> Request -> BodyReader -> Int -> IO L.ByteString
brReadSomeWithTimeout tmo req br = brReadSome $ brReadWithTimeout tmo req br

-- | This is like 'brConsume' but with a timeout between body read events.
--
-- The value of the timeout is passed in the first parameter as a number of
-- microseconds. A negative value effectively disables the timeout which makes
-- the function behave exactly as 'brConsume'.
--
-- Implemented as `brConsume` with `brReadWithTimeout` passed in its first
-- parameter.
--
-- @since 0.2.0.0
brConsumeWithTimeout :: Int -> Request -> BodyReader -> IO [ByteString]
brConsumeWithTimeout tmo req br = brConsume $ brReadWithTimeout tmo req br

-- | This is like 'httpLbs' but with a timeout between body read events.
--
-- The value of the timeout is passed in the first parameter as a number of
-- microseconds. A negative value effectively disables the timeout which makes
-- the function behave exactly as 'httpLbs'.
httpLbsBrReadWithCustomTimeout :: Int -> Request -> Manager ->
    IO (Response L.ByteString)
httpLbsBrReadWithCustomTimeout tmo req man = withResponse req man $ \res -> do
    bss <- brConsumeWithTimeout tmo req $ responseBody res
    return res { responseBody = L.fromChunks bss }

-- | This is like 'httpLbs' but with a timeout between body read events.
--
-- The value of the timeout is retrieved from the t'ResponseTimeout' of the
-- request.
httpLbsBrReadWithTimeout :: Request -> Manager -> IO (Response L.ByteString)
httpLbsBrReadWithTimeout req man =
    httpLbsBrReadWithCustomTimeout (fromResponseTimeout req man) req man

