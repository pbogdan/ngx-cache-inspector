# ngx-cache-inspector

Inspect headers and bodies of nginx's cache files. Currently cache files marked as version 3 and 5 are supported.

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
$ nci inspect file [--dump-body]
```

Inspect the header of a single cache file.

#### Flags

- `--dump-body` - an optional flag that can be used to dump the cached response body stored in the file. `nci` will attempt to decompress the body, if that fails it will dump the raw result.

### Searching for cache files mathching a predicate.

```
$ nci find path (--key needle | --valid-until [+-]date | --created [+-]date | --etag needle | --vary needle) ([--and] | [--or]) [--inspect]
```

Find cache files matching the flags.

`path` specifies the location of the cache directory.

#### Flags.

At least one of `--key`, `--valid-until`, `--created`, `--etag`, `--vary` flags needs to be specified. When more than one flag is specified they will be combined with "and" semantics. For example:

```
$ nci find /srv/nginx/cache --key example.com --valid-until 2017-03-06
```

will search for cache files with the key containing `example.com` string that are due to expire on `2017-03-06`. This can be overridden with the `--or` flag, this will search for files matching any of the conditions.

The arguments passed to `valid-until` and `created` flags accept an optional `+` or `-` prefix. If not specified an exact match will performed. `+` allows searching for values that are greater than the argument, with `-` searching for values that are lower than the argument. For example:

```
$ nci find /srv/nginx/cache --valid-until 2017-03-06 00:00:00
```

will find cache files that are due to expire exactly on `2017-03-06 00:00:00`.

```
$ nci find /srv/nginx/cache --valid-until "+2017-03-06 00:00:00"
```

will find cache files that are due to expire __after__ `2017-03-06 00:00:00`. And

```
$ nci find /srv/nginx/cache --valid-until "-2017-03-06 00:00:00"
```

will find cache files that are due to expire __before__ `2017-03-06 00:00:00`.

`nci` supports the same date formats as git.

- `--key needle`- search for needle within the cache key.
- `--valid-until [+-]date` -  compare valid until date against the argument.
- `--created [+-]date` - compare created date against the argument.
- `--etag needle` - search for needle within the etag.
- `--vary needle` - search for needle within vary.
- `--and` - combine the flags with "and".
- `--or` - combine the flags with "or".
- `--inspect` - optionally dump the header of each matched cache file

## License

BSD3
