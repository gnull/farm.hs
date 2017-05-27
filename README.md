Program for running an exploit on many opponent teams in parallel, collecting
flags and sumbitting them to a jury checker.

Usage
=====

```
$ farm-hs --help
Helper program for Attack-Defence CTF competitions

Usage: farm-hs PROGRAM [[ARG1 [ARG2 ...]]] (-t|--targets FILE)
               (-s|--submit COMMAND) (-j|--jobs N) (-d|--delay SECS)
               (-r|--flagre REGEX)
  Run an exploit on list of target hosts

Available options:
  PROGRAM                  Exploit program
  [ARG1 [ARG2 ...]]        Arguments for exploit PROGRAM
  -t,--targets FILE        File containing list of target IP addresses
  -s,--submit COMMAND      Shell command to submit flags. This command will get
                           flag in `flag` environment variable. E.g. `echo
                           $flag`
  -j,--jobs N              Number of parallel running exploit jobs
  -d,--delay SECS          Number of seconds to wait after each exploit run
  -r,--flagre REGEX        POSIX regex for flag
  -h,--help                Show this help text
```

`farm-hs` can also generate bash completion for itself. Feeding it into your bash
is straightforward:

```sh
source <(farm-hs --bash-completion-script `which farm-hs`)
```

You can add this line directly into your `.bashrc`.
               
Installation
============

If you don't want to bother yourself building `farm-hs` from source check out
the [releases](https://github.com/gnull/farm.hs/releases) page. It contains
prebuilt static binaries for x86 and x86-64.

`farm-hs` can be built and installed either in `~/.cabal/bin` using
`cabal-install`:

```sh
cabal build
cabal install
```

or in `~/.local/bin` using `stack`:

```sh
stack setup
stack build
stack install
```
