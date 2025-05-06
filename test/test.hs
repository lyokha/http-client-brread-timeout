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

testRequest :: (Request -> Manager -> IO (Response ByteString)) ->
    Request -> Manager -> ResponseStatus -> Test
testRequest handler req man expected = TestCase $ do
    status <- (ResponseStatus . statusCode . responseStatus <$>
                  handler req man
              ) `catch` \case
                  HttpExceptionRequest _ ResponseTimeout ->
                      return ResponseTimeoutException
                  _ -> return ResponseOtherException
    status @?= expected

main :: IO ()
main = do
    man <- newManager defaultManagerSettings
    reqVerySlow <- parseRequest "GET http://127.0.0.1:8010/very/slow"
    reqSlow <- parseRequest "GET http://127.0.0.1:8010/slow"

    runTestTTAndExit $ TestList
        [TestLabel "testHttpLbs" $
            testRequest httpLbs reqVerySlow man $
                ResponseStatus 200
        ,TestLabel "testHttpLbsBrReadWithTimeout" $
            testRequest httpLbsBrReadWithTimeout reqVerySlow man
                ResponseTimeoutException
        ,TestLabel "testHttpLbsBrReadWithTimeout" $
            testRequest httpLbsBrReadWithTimeout reqSlow man $
                ResponseStatus 200
        ]

