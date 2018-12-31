wsjtx-to-mqtt
=============

[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Build Status][badge-travis]][travis]
[![AppVeyor][badge-appveyor]][appveyor]
[![Windows Binary Release][badge-github-releases]][github-releases]

`wsjtx-to-mqtt` is a small UDP server that listens for incoming UDP packages from WSJT-X.
It supports several modes of operations:

* `wsjtx-to-mqtt forward` : forward UDP packages to a MQTT server (0.1.5.0 does not support MQTT on Windows)
* `wsjtx-to-mqtt dumpWsjtx --format DumpText` : print incoming packages in a text format
* `wsjtx-to-mqtt dumpWsjtx --format DumpJSON` : print packages in JSON
* `wsjtx-to-mqtt showConfig` : print a template for the config file
* `wsjtx-to-mqtt --help` : help message
* `wsjtx-to-mqtt sendToWsjtx` : send a command to WSJT-X

The `wsjtx-udp` library is used for parsing UDP packages.
It works with WSJT-X version 2.0.0 but does not support some of the extension of 2.0.0 over 1.9.0.

[travis]: https://travis-ci.org/MarcFontaine/wsjtx-to-mqtt
[badge-travis]: https://img.shields.io/travis/MarcFontaine/wsjtx-to-mqtt.svg?label=Linux%20build
[appveyor]: https://ci.appveyor.com/project/MarcFontaine/wsjtx-to-mqtt/branch/master
[badge-appveyor]: https://img.shields.io/appveyor/ci/MarcFontaine/wsjtx-to-mqtt.svg?label=Windows%20build
[badge-github-releases]: https://img.shields.io/github/release/MarcFontaine/wsjtx-to-mqtt.svg?label=Windows%20Binary
[github-releases]: https://github.com/MarcFontaine/wsjtx-to-mqtt/releases
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg
[license]: https://github.com/MarcFontaine/wsjtx-udp/blob/master/LICENSE
[hackage]: https://hackage.haskell.org/package/wsjtx-to-mqtt
[badge-hackage]: https://img.shields.io/hackage/v/wsjtx-to-mqtt.svg
