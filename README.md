<h1 align="center">
    <a href="https://github.com/weak-head/pymble">
        pymble
    </a>
</h1>

<p align="center">
    <a href="https://travis-ci.org/weak-head/pymble">
        <img alt="Build Status"
             src="https://travis-ci.org/weak-head/pymble.svg?branch=master">
    </a>
    <a href="https://coveralls.io/github/weak-head/pymble?branch=master">
        <img alt="Coverage Status"
             src="https://coveralls.io/repos/github/weak-head/pymble/badge.svg?branch=master">
    </a>
    <a href="https://github.com/weak-head/pymble/blob/master/LICENSE">
        <img alt="License: MIT"
             src="https://img.shields.io/badge/license-MIT-blue.svg">
    </a>
</p>

<p align="center">
  pymble is a telnet server that converts images and graphics into colored ASCII art that is rendered on any ANSI compatible terminal
</p>

<hr>

![futurama](/img/futurama.png)

<hr>

## Usage

By default, if started without any arguments, pymble runs as a telnet server and listens to incoming connections on port 23. To change the default port just specify it explicitly on the startup:

``` sh
$ pymble -p 42
```

You can use pretty much any telnet client to connect to the server. Once connected type `help` for the usage information:

![telnet-usage](/img/telnet-usage.png)

The image is rendered directly in your terminal and you are limited to the capabilities of your telnet client. Some telnet clients do not support TrueColor, others could alter the color a little bit. Also there is a [minor inconsistency](https://en.wikipedia.org/wiki/ANSI_escape_code#Colors) of the default colors in the different terminals/clients. So just keep this in mind if you want to have a good color accuracy.

Another thing to consider is the font that is used to render the ASCII art. In case if you are relying on the automatic ASCII art width/height detection, all the image transformations and approximations would be done for the font with the aspect ratio of `0.58`. Mostly it works OK for `monofur`, `Menlo` or `Verdana`, but if you are using `Courier New` or something like `Times New Roman` they have a completely different aspect ratio and the resulting ASCII art would be stretched. So if you dont have a desired result, the good idea would be to explicitly specify the width and the height of the ASCII art, basically you should find the rendering settings that work the best for your setup.

### Command-line usage

You can convert an image to ASCII art directly from the command line without a need to run the telnet server.

``` sh

# show help and usage information
$ pymble --help

# render 40x40 TrueColor art
$ pymble -c tc -w 40 -h 40 image.png

# default size, grayscale
$ pymble --color gs /home/image.png

# fixed width, flexible height, 16 colors
$ pymble -c 16 -w 80 http://some/url/img.png

```

## Building pymble

Pymble is build using [Haskell Tool Stack](haskellstack.org). To build pymble, run:

``` sh

# build pymble
$ stack build

# run test cases
$ stack test

# run pymble
$ stack exec pymble

```

Alternatively you can build and run pymble telnet server as a docker container:

``` sh

# build pymble container
$ docker build -t pymble .

# run pymble telnet server
$ docker run -p 23:23 -d pymble

```
