hummingbird MQTT broker [![Hackage](https://img.shields.io/github/release/lpeterse/haskell-hummingbird.svg)](https://github.com/lpeterse/haskell-hummingbird/releases) [![Travis](https://img.shields.io/travis/lpeterse/haskell-hummingbird.svg)](https://travis-ci.org/lpeterse/haskell-hummingbird)
=======================

This is a multi-threaded MQTT broker (framework) written in Haskell.

It is based on the [Haskell MQTT](https://github.com/lpeterse/haskell-mqtt) library.

## Batteries included

The package installs `hummingbird` which is already a full-fledged MQTT
broker with configuration files, logging, authentication etc.

If this is not sufficient for your use case you might also consider to
assemble your own broker (i.e. with a custom authentication mechanism).

## Customized brokers

The package exports modules which make it easy to compile a custom
broker executable. Look into the `hummingbird` implemention for getting
started!

## Creating a self-signed certificate for client authentication

The following script creates a 2048 bit RSA keypair and a self-signed
certificate to be used by the client for authentication.

The server only needs to know the public key. It will accept any certificate
signed with the corresponding private key and doesn't use any other information
from the certificate.

```bash
#!/bin/bash

set -e

openssl genrsa -out private.pem 2048
openssl rsa -in private.pem -outform PEM -pubout -out public.pem
openssl req -new -sha256 -key private.pem -out csr.csr -subj "/CN=$1"
openssl req -x509 -sha256 -days 3650 -key private.pem -in csr.csr -out certificate.pem
openssl x509 -text -in certificate.pem
```

## License

Permission is hereby granted under the terms of the MIT license:

> Copyright (c) 2016 Lars Petersen
>
> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be included
> in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
> IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
> CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
> TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
