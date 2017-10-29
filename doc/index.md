# Buildsome

Buildsome is a generic build tool which provides emphasis on correctness
while keeping the ease of use. The guiding principle is that partial
rebuilds should be fast, and always yield outputs that are identical
to full builds, regardless of the complexity of the build tree.

To facilitate this, Buildsome adds advanced features, such as
automatic dependency detection, output tracking, and more. Folks
familiar with [make](https://www.gnu.org/software/make) should feel at
home.

## Highlights

* **Automatic, generic, dependency handling**: no need to specify
  input dependencies for targets, as dependencies are detected while
  targets are running, and the dependencies are remembered for further
  execution. This makes specifing targets much easier. Only specify
  the outputs. Input specification is optional.
* **Correctness**: detection of extraenous file modification during
  build (for example, by a user's editor).
* **Output capture**: The standard output and standard error of
  execution are saved for replay and diagnosis.
* **Verbosity**: prints in a friendly manner why a target was rebuilt.
* **Git integartion**: a root `.gitignore` file is automatically
  maintained for all build outputs, so no 'clean' targets are needed.
* The target specification itself is implicitly a dependency, unlike
  in [make](https://www.gnu.org/software/make).
* Out of tree (global) dependencies are detected too under the same
  mechanism. For instance, if you reinstalled `/usr/bin/gcc`, it will
  be detected.

## In depth

### Convenience

With Buildsome, you're free to specify your inputs *partially* (such
specification can aid a more efficient build process). You do not have
to specify your file system inputs explicitly though, as they're
automatically detected using file system access hooks.

### No false negatives

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
have a different result, and Buildsome will detect this and re-execute
the compilation command, unlike other build systems.

Another correctness issue solved by Buildsome is meddling with the
file system *during* a build process. Unlike other build systems,
Buildsome detects this and will not record false build results in
detectable cases. The only undetectable cases are third-party meddling
with outputs of a command while it is being built. Editing a source
file immediately after it was fed to a compiler will not accidentally
mark the compilation as "newer" than that source file.

### No lost warnings

Virtually all build systems do not capture stdout/stderr of executed
commands. When a command succeeds (e.g: a compiler invocation), its
stdout and stderr are displayed once and then forgotten. All future
builds will never present the warnings from the compiler, which are
thus lost until a "clean" command is issued. Buildsome captures these
outputs and re-prints them when an execution is cached. This avoids
the most common reason "-Werror" is used: as warnings are no longer
lost.

### Parallelism

Automatic detection of file system dependencies and parallelism are
somewhat contradictory. To overcome this, Buildsome uses speculative
parallelism based on previous detected inputs. These are likely to be
