### 0.3.0.0

- Functions *brReaWithTimeout*, *brReadSomeWithTimeout*, and
  *brConsumeWithTimeout* now throw clean *ResponseTimeout* without wrapping in
  the request data.
  + **Breaking changes**: functions *brReaWithTimeout*, *brReadSomeWithTimeout*,
    and *brConsumeWithTimeout* no longer accept *Request* parameter.

### 0.2.0.0

- Added functions *brReadSomeWithTimeout* and *brConsumeWithTimeout*.

### 0.1.1.1

- Refactor module imports.

### 0.1.1.0

- Added function *httpLbsBrReadWithCustomTimeout*.

### 0.1.0.0

- Initial version.

