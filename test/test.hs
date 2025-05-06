import Network.HTTP.Client
import Network.HTTP.Client.BrReadWithTimeout
import Network.HTTP.Types.Status
import Data.ByteString.Lazy (ByteString)
import Control.Exception
import Test.HUnit

testReq :: (Request -> Manager -> IO (Response ByteString)) ->
    Request -> Manager -> Int -> Test
testReq handler req man expected = TestCase $ do
    status <- (statusCode . responseStatus <$> handler req man) `catch` \e ->
        return $ case e of
                     HttpExceptionRequest _ ResponseTimeout -> 504
                     _ -> 500
    status @?= expected

main :: IO ()
main = do
    let httpManager = newManager defaultManagerSettings
    man <- httpManager
    reqVerySlow <- parseRequest "GET http://127.0.0.1:8010/very/slow"
    reqSlow <- parseRequest "GET http://127.0.0.1:8010/slow"

    runTestTTAndExit $ TestList
        [TestLabel "testHttpLbs" $
            testReq httpLbs reqVerySlow man 200
        ,TestLabel "testHttpLbsBrReadWithTimeout" $
            testReq httpLbsBrReadWithTimeout reqVerySlow man 504
        ,TestLabel "testHttpLbsBrReadWithTimeout" $
            testReq httpLbsBrReadWithTimeout reqSlow man 200
        ]

