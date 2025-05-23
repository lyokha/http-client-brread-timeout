{-# LANGUAGE LambdaCase #-}

import Network.HTTP.Client
import Network.HTTP.Client.BrReadWithTimeout
import Network.HTTP.Types.Status
import Data.ByteString.Lazy (ByteString)
import Control.Exception
import Test.HUnit

data ResponseStatus = ResponseStatus Int
                    | ResponseTimeoutException
                    | ResponseOtherException
                    deriving (Show, Eq)

testRequest :: IO (Response ByteString) -> ResponseStatus -> Test
testRequest req expected = TestCase $ do
    status <- (ResponseStatus . statusCode . responseStatus <$> req)
        `catch` \case
                    HttpExceptionRequest _ ResponseTimeout ->
                        return ResponseTimeoutException
                    _ -> return ResponseOtherException
    status @?= expected

main :: IO ()
main = do
    man <- newManager defaultManagerSettings
    reqVerySlow <- parseRequest "GET http://127.0.0.1:8010/very/slow"
    reqSlow <- parseRequest "GET http://127.0.0.1:8010/slow"

    runTestTTAndExit $ test
        ["httpLbs-reqVerySlow" ~:
            testRequest (httpLbs reqVerySlow man) $
                ResponseStatus 200
        ,"httpLbsBrReadWithTimeout-reqVerySlow" ~:
            testRequest (httpLbsBrReadWithTimeout reqVerySlow man)
                ResponseTimeoutException
        ,"httpLbsBrReadWithTimeout-reqSlow" ~:
            testRequest (httpLbsBrReadWithTimeout reqSlow man) $
                ResponseStatus 200
        ]

