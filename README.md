# ngx-cache-inspector

Inspect headers and bodies of nginx's cache files. Currently only cache file marked as version 3 are supported.

Example output of inspecting a cache file:

```
Version:             3
Valid until:         Wed Mar  1 19:09:18 UTC 2017
Last modified:       Wed Dec 31 23:59:59 UTC 1969
Created:             Wed Mar  1 18:39:18 UTC 2017
CRC32:               2863228699
Valid msec:          0
Header start offset: 195
Body start offset:   476
ETag length:         0
ETag:                
Vary len:            15
Vary:                Accept-Encoding
Variant:             some variant
Cache key:           your cache key
```

The code itself is a thin CLI wrapper around [parsing facilities included in ngx-cache-purge](https://github.com/pbogdan/ngx-cache-purge/blob/master/src/Cache/Header.hs).

## Installation

Grab yourself a binary from the [releases page](https://github.com/pbogdan/ngx-cache-inspector/releases) or build your own from source - see below.

## Building from source

The easiest way to build the project is using [haskell stack tool](https://docs.haskellstack.org/en/stable/README/):

```
$ git clone https://github.com/pbogdan/ngx-cache-inspector.git
$ cd ngx-cache-inspector
$ stack build
```

the executable can then be run with:

```
$ stack exec -- nci
```

To find the location of `nci` you can use:

```
$ stack exec -- which nci
```

## Usage

### Inspecting a single file

```
$ nci FILE [--dump-body]
```

The optional `--dump-body` flag can be used to dump the cached response body stored in the file.

### Searching for cache files with matching keys.

```
$ nci find PATH NEEDLE
```

PATH specifies the location of the cache directory, and NEEDLE is the string to search for within the key.

## License

BSD3
