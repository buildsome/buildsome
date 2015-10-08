# Installation instructions

To install buildsome follow [these](install.md) instructions.

# Introduction

Tired of cryptic bugs in the build system? Of ever having to run "make
clean"? Tired of manually specifying dependencies and introducing
ad-hoc dependency scanners? Then buildsome is for you!

Buildsome is an innovative build system, meant to both ease the
declaration of the build steps, and give better guarantees to users.

As long as the build steps being used depend only on inputs from the
file system, buildsome guarantees that the build is correct and
determinstic.

The dependencies/inputs are automatically detected.

# Convenience

With buildsome, you're free to specify your inputs *partially* (such
specification can aid a more efficient build process). You do not have
to specify your file system inputs explicitly though, as they're
automatically detected using file system access hooks.

# Correctness

## No false negatives

Automatic detection of dependencies does not only add convenience, it
also guarantees correctness. Correctness means no "false negative"
builds. "False negative" means that when inputs change, the build
system does not always recognize this properly, and avoids rebuilding
parts that require it. Such false negatives plague virtually every
build system in existence.

As an example: Virtually all build systems use `gcc -M` to detect
`#include` dependencies.  If gcc was invoked with `-Ia -Ib`, and
`example.h` was included and found in `b/example.h`, most build
systems will only know to rebuild if `b/example.h` is changed.  If
`a/example.h` is introduced, however, the compilation process will
have a different result, and buildsome will detect this and re-execute
the compilation command, unlike other build systems.

Another correctness issue solved by buildsome is meddling with the
file system *during* a build process. Unlike other build systems,
buildsome detects this and will not record false build results in
detectable cases. The only undetectable cases are third-party meddling
with outputs of a command while it is being built. Editing a source
file immediately after it was fed to a compiler will not accidentally
mark the compilation as "newer" than that source file.

# No lost warnings

Virtually all build systems do not capture stdout/stderr of executed
commands. When a command succeeds (e.g: a compiler invocation), its
stdout and stderr are displayed once and then forgotten. All future
builds will never present the warnings from the compiler, which are
thus lost until a "clean" command is issued. Buildsome captures these
outputs and re-prints them when an execution is cached. This avoids
the most common reason "-Werror" is used: as warnings are no longer
lost.

## Parallelism

Automatic detection of file system dependencies and parallelism are
somewhat contradictory. To overcome this, buildsome uses speculative
parallelism based on previous detected inputs. These are likely to be
the inputs of the new executions as well, so are speculatively built.

# Future plans

## Caching

The global file system hooks can facilitate a generalized "ccache",
that works correctly with more than just a C compiler. It is planned
to support caching of all build results, rather than just the last
one. Switching between debug and release builds should not require
rebuilding everything, if such versions were built in the
past. Additionally, "global" caching should be available: On a company
LAN with many builders, it is often more efficient to pull build
results from the network than to rebuild them locally.

## Target parameterization

Targets should be more like functions that can be parameterized. File
names will have an auto-generated hash component in them to represent
the parameters used. This will allow, for example, painlessly building
the same libraries in various different configurations to link into
different executables without duplicating the libraries' declarations.

## Better speculative parallelism

When speculative parallelism is detected as unnecessary, buildsome
will terminate the speculations.

# Installation

Use "cabal install" to build and install buildsome.

# Credits

The file system hooks are inspired by
[tup](http://gittup.org/tup/). They are taken a couple of steps
further though, by hooking the file system globally, and not just
locally, avoiding false negatives relating to system-wide changes,
too. These hooks are also used to auto-detect dependencies and delay
dependent executions, rather than just verify explicitly specified
dependencies.
