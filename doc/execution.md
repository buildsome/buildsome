# Execution

To see an overview of all command line options, run `buildsome --help`. This
document will focus on most significant aspects of the command line options,
and operation around building.

## Specifying targets

One or more target can specified at the command line, or none at all (explained
here);

```no-highlight
buildsome
```

Or:

```no-highlight
buildsome some-file-to-build subdir/some/nested/target
```

Or from inside a subdirectory:

```no-highlight
cd subdir/some
buildsome nested/target
```

By default, `Buildsome` regards the directory containing the top-most
`Buildsome.mk` as the root dir, and always starts evaluating the
target tree from there. Additionally, running `buildsome` without specifying
targets, while in any sub-directory, is equivalent to trying to build
the `default` target in that sub-directory.

### Building in sub-directory while being in it, or outside of it

It may be confusing that passing a source directory to `buildsome` as
a target does nothing. That happens because the directory itself
already exists, so nothing needs to be done. Instead, if one defines a
`default` target in a subdirectory, it would make more sense to pass
that target explicity, e.g. `buildsome subdir/default`.

## Output format

Buildsome makes an effort to tell why a target is being built. There
could be various reasons:

* Rebuild due to changes in the content or meta-data of an input.

```no-highlight
T141 Execution log of ["src/.objs/Class.c.release.o"] did not match because input(content): change: "src/Class.c"
T141 { ["src/Class.o"] BUILDING (BecauseSpeculative (BecauseHooked AccessDocEmpty))
```
* Output is missing

```no-highlight
T093 Execution log of ["src/Class.o"] did not match because output file was deleted: "src/Class.o"
T093 { ["src/Class.o"] BUILDING (BecauseSpeculative (BecauseHooked AccessDocEmpty))
```

* Rebuild due to changes in the evaulated target script.

```no-highlight
T093 { ["src/Class.o"] BUILDING (BecauseSpeculative (BecauseHooked AccessDocEmpty))
```

**(TODO: it does not explicitly say that a target script was changed.
This needs fixing)**.

## Execution warnings

### Over-specified inputs

If a target depends on a file explicitly in the definition, but does not
make use of that file as an input during execution, this warning is
emitted. It is important for targets to confirm to their minimal
specification

## Execution errors

Various errors are encountered by users of Buildsome. The following is
a summary of the major ones, and detailed explaination.

### ThirdPartyMeddlingError

Prior to letting target running under Buildsome to open their dependencies,
Buildsome checks the modification time of the input files at the leaf of
the dependencies tree. If any of these sources files change during the,
this error is emitted.

This feature is meant to detected accidental user edits during execution.
Without this build, in a compilation which consists of several files may
result in an invalid build, because some of the targets received inputs
that don't match the inputs of other targets. This is especially valid
for C headers, for example.

### TargetCommandFailed

This error indicates that one of the target scripts returned a non-zero
status code and likely failed. For example, this is the UNIX behavior
to signify process execution failure.

### UnregisteredOutputFileExists

Because Buildsome tracks the output files of all targets (removing
the necessity of manually maintaing a old fashioned 'clean' target),
then it mindful about finding files that were not created by any
target in a previous invocation.

This could happen if Buildsome is executed and for some reason later,
the database that it maintains to track executions gets deleted. The
database is `Buildsome.mk.db`, and it resides at the top directory,
where `Buildsome.mk` is located. It is possible to provide the
`--overwrite` switch in that case, to suppress this behavior. It
should not be the default way of executing Buildsome, in any case.

### TargetDependencyLoop

When Buildsome detects that a target directly or indirectly depends on
itself, whether implicitly or explicitly, this error is emitted. Note
that directory listing by processes introduces a dependency over the
directory that is being listed. If the same process also modifies that
directory, it would constitued a dependency loop.

## Parallelism

It is possible to pass a `-j` parameter that determines the number of
jobs to run in parallel. Note that it does not serve as a hard limit,
because it is possible that jobs would be temporarily stopped
mid-execution in order to satisfy dependencies, so `-j` comes to serve
as a limit to the number of jobs currently not waiting for any
dependency.

## Bypassing automatic dependency detection

Under Buildsome, certain types of targets are problematic. For example,
targets that maintain their own caching, meaning they arbitrarily read
inputs from a previous invocation. You may find the complex 'do-all'
tools such as `go build` (of the Go language), `mvn` (Maven), or `ccache`
may behave in such a way.

It is possible to suppress the dependency scanning of Buildsome by
overriding the `LD_PRELOAD` variable during target execution.

**NOTE: One must be careful when doing so, because it means that inputs
would not be detected, and later, unbeknownst to the developer, actual
dependencies would not match the specification that Buildsome infers,
regressing back to gmake-like hell.**

### Example pattern for dependency detection bypass

Suppose we have a program named `problematic-command` which performs
problematic file system accesses during its execution. Suppose that
we know for a fact that it only reads to inputs `input-a` and `input-b`.

In that case, we can use shell script to let it execute under Buildsome,
but still hint Buildsome regarding the outputs and inputs of the whole
target, using file accesses of our own. Our shell script can appear as
such:

```no-highlight
#!/bin/bash

cat input-a input-b > /dev/null
LD_PRELOAD= problematic-command output-tmp
cat output-tmp > actual-output
```

And in the `.mk` file:

```no-highlight
actual-output:
        script.sh
```
