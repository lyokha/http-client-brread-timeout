Http client with time-limited brRead
====================================

[![Build Status](https://github.com/lyokha/http-client-brread-timeout/workflows/CI/badge.svg)](https://github.com/lyokha/http-client-brread-timeout/actions?query=workflow%3ACI)
[![Hackage](https://img.shields.io/hackage/v/http-client-brread-timeout.svg?label=hackage%20%7C%20http-client-brread-timeout&logo=haskell&logoColor=%239580D1)](https://hackage.haskell.org/package/http-client-brread-timeout)

Http client with timeouts applied in between body read events.

Note that the response timeout in
[*http-client*](https://github.com/snoyberg/http-client) is applied only when
receiving the response headers which is not always satisfactory given that a
slow server may send the rest of the response very slowly.

### How do I test this?

A slow server can be emulated in *Nginx* with the following configuration.

```nginx
user                    nobody;
worker_processes        2;

events {
    worker_connections  1024;
}

http {
    default_type        application/octet-stream;
    sendfile            on;

    server {
        listen          8010;
        server_name     main;

        location /slow {
            # send a single response body chunk once in 20 sec
            echo 1;
            echo_flush;
            echo_sleep 20;
            echo 2;
            echo_flush;
            echo_sleep 20;
            echo 3;
            echo_flush;
            echo_sleep 20;
        }

        location /very/slow {
            echo 1;
            echo_flush;
            echo_sleep 20;
            # chunk 2 is extremely slow (40 sec)
            echo 2;
            echo_flush;
            echo_sleep 40;
            echo 3;
            echo_flush;
            echo_sleep 20;
        }
    }
}
```

*GHCI* session.

<pre>
<b>Prelude&gt;</b> import Network.HTTP.Client as HTTP.Client
<b>Prelude HTTP.Client&gt;</b> import Network.HTTP.Client.BrReadWithTimeout as BrReadWithTimeout
<b>Prelude HTTP.Client BrReadWithTimeout&gt;</b> httpManager = newManager defaultManagerSettings
<b>Prelude HTTP.Client BrReadWithTimeout&gt;</b> man &lt;- httpManager
<b>Prelude HTTP.Client BrReadWithTimeout&gt;</b> reqVerySlow &lt;- parseRequest "GET http://127.0.0.1:8010/very/slow"
<b>Prelude HTTP.Client BrReadWithTimeout&gt;</b> reqSlow &lt;- parseRequest "GET http://127.0.0.1:8010/slow"
<b>Prelude HTTP.Client BrReadWithTimeout&gt;</b> :set +s
<b>Prelude HTTP.Client BrReadWithTimeout&gt;</b> httpLbs reqVerySlow man
Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("Server","nginx/1.22.0"),("Date","Wed, 22 Jun 2022 03:54:43 GMT"),("Content-Type","application/octet-stream"),("Transfer-Encoding","chunked"),("Connection","keep-alive")], responseBody = "1\n2\n3\n", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
  host                 = "127.0.0.1"
  port                 = 8010
  secure               = False
  requestHeaders       = []
  path                 = "/very/slow"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
}
(80.11 secs, 1,087,472 bytes)
<b>Prelude HTTP.Client BrReadWithTimeout&gt;</b> httpLbsBrReadWithTimeout reqVerySlow man
&ast;&ast;&ast; Exception: HttpExceptionRequest Request {
  host                 = "127.0.0.1"
  port                 = 8010
  secure               = False
  requestHeaders       = []
  path                 = "/very/slow"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutMicro 30000000
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
 ResponseTimeout
<b>Prelude HTTP.Client BrReadWithTimeout&gt;</b> httpLbsBrReadWithTimeout reqSlow man
Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("Server","nginx/1.22.0"),("Date","Wed, 22 Jun 2022 03:57:20 GMT"),("Content-Type","application/octet-stream"),("Transfer-Encoding","chunked"),("Connection","keep-alive")], responseBody = "1\n2\n3\n", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
  host                 = "127.0.0.1"
  port                 = 8010
  secure               = False
  requestHeaders       = []
  path                 = "/slow"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
}
(60.07 secs, 1,077,320 bytes)
</pre>

Here, the first request comes from the standard `httpLbs` which, after timely
receiving of the first chunk of the response (including headers and the first
chunk of the body), no longer applies any timeouts and may last as long as the
response endures: in this case, it lasts 80 seconds and successfully returns.
In the second request, `httpLbsBrReadWithTimeout` timely receives the first
chunk of the response too, however the second chunk is coming in 40 seconds
which exceeds the default response timeout value (30 seconds), and the function
throws `ResponseTimeout` exception after 50 seconds from the start. In the third
request, `httpLbsBrReadWithTimeout` returns successfully after 60 seconds
because every chunk of the response comes every 20 seconds without triggering
timeouts.

