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
