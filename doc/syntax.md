## 'Make' compatibility

The syntax for Buildsome's definition files is similar tho that of
[make](https://www.gnu.org/software/make), however only a small subset
is implemented, and a few constructs are added. The differences are
described here.

## Directory structure

Unlike `make`, Buildsome is designed for a directory structures from
the get-go. Therefore, when executing in any directory, it looks up
the directory structure to find the top-most `Buildsome.mk` file. It
starts the evaluation process from there.

## Purity

The evalution of Buildsome's definition files, which is needed for the
process of devising a target tree, is done in a precise way that is
completely independent of the state of other files in the directory
structure. It is therefore not possible to generate these definition
files during the execution of Buildsome itself, and there is no `make`
like process of "Makefile-reloading". This limitiation is intentional.

## Macros

### Macro assignment

There are currently two ways to define a macro, shown in the next example:

```no-highlight
FOO = value
BAR ?= value
```

In this case variable `FOO` will be assigned with the new string
always, and `BAR` will be assigned only if it was not assigned before.

### Command line build customization

There is a special case concerning variables whose names start with
`FLAG_`: If `FLAG_X` is defined with with `?=`, then the user can pass
`--with X` in the command line when executing Buildsome in order to
set an initial value `enabled`, or `--without` to set it to
`disabled`.

### Macros are unrelated to POSIX environment

Unlike in `make` there is *no relation between macros in Buildsome
definition files and POSIX environment variables*. In fact, when
target are executing their commands, it is done with a very minimal
environment variable set.

The POSIX environment is made pristine for target execution. For
example, except `$PATH` and `$HOME`, any environment variable set
in `.bashrc`, will not appear set under target execution. The reason
is to ensure reproducability of the build between users, each having
their own possibly environment-sensitive tools under a different
execution environment.

### Scope saving

Using `local {` and `local }`, it is possible to save-restore the
values of all variables.

```no-highlight
A = 2
local {
A = 5
local }
```

In the example above, `A` will revert to the value `2` after the
`local }` line.

### Substitution

Macros can refer to other macros via `${...}`. The expansion itself
however takes place only in the target definition.

```no-highlight
CFLAGS=-O2 ${CFLAGS_FEATURE}
```

### Cartesian expansion

Similarly to expansion done in some shells, comma-delimited expressions
enclosed in curly braces are expanded. For example, the expression
`x{a,b,c}` is expanded to `xa xb xc`.

## Multiline assignments

Using `\`, it is possible to extend a macro assignment to multiple
lines. For example:


```no-highlight
DEFAULT_CFLAGS_C_COMMON=      \
    ${DEFAULT_CFLAGS_COMMON}  \
    ${CFLAGS_POISON_FULLY}    \

```

## Include directives

It is possible to recursively include other files. For example:

```no-highlight
include otherdir/otherfile.mk
```

## Conditional evaluation

Currently, `make`-style `ifeq` and `ifneq` are supported, along with
`else`.


```no-highlight
CLFAGS_FEATURE=
ifeq ($(flag_BAR),enabled)
CLFAGS_FEATURE=-DFEATURE
endif
```

There is a special case with variable whose names start with
`FLAG_`. If those are defined with `?=`, then the user can pass
`--with` in the command when executing buildsome in order to set an
initial value `enabled`.

## Target definition

Similarly to `make`, the syntax for targets is as follows:

```no-highlight
<outputs> : <optionally-specified-inputs> (| <order-only-inputs>)
<tab char><script line 1>
<tab char><script line 2>
<tab char><script line n>
```

Not that unlike in `make`, all script lines are executed as one shell
script, instead of separately.

The target named `default` is built if no target is mentioned in the
command line.

Macro expansion takes place at the target line specification, and in
it associated shell script.

Inputs can be specified in order to assist in parallel first builds.
However, if the execution of the target does make use of these inputs,
a warning is emitted.

### Simple patterns

Target patterns can be specified similarly to the `make` syntax. One
or more files can be outputs, and `%` serves as a wild card. For
example:

*(note that in the `cpp` to `o` rule below, nothing more needs to be specified, as all other input dependencies such as included headers
are automatically detected.)*

```no-highlight
%.o: %.cpp
        ${COMPILEXX}
```

However, there is an important difference between the `mae` and
Buildsome functionality of patterns. In the example above, the pattern
does not match targets in a subtree (e.g .`subdir/a.o`), but only in
the project's root directory. Though it is possible to reuse patterns,
albeit in an explicit manner–a topic we shall visit in the next
section.

#### Reuse of patterns in sub-directories

In order to make a pattern apply in more than one directory, we can
use combine *Scope saving*, *Include directives*, and *variable
assignment*s in the following manner.

**build/patterns.mk**:
```no-highlight
${curdir}/%.o: ${curdir}/%.cpp
        ${COMPILEXX}
```

**Buildsome.mk**:
```no-highlight
curdir=.
include patterns/include.mk
include subdir/include.mk
```

**subdir/include.mk**:
```no-highlight
local {

curdir=subdir
include patterns/include.mk

local }
```

Note the assignment of the Buildsome variable `curdir`, that serves as
the path to the current directory. It is more common to use the valid
variable name `.` instead of the lengthy `curdir`, so that `$.` can be
used inplace of `${curdir}`.

### Wildcard patterns

Wildcard patterns allow a one-to-many target instanitation, by adding
a wildcard in the output side of a pattern target.

For example:

```no-highlight
local_%.sep.*.c: %.foo
        ${TEMPLATE_MAKER}
```

The rule above will match for files names which match the globbing
pattern `local_*.sep.*.c`. For example, with the file `local_test.sep.bar.c`,
the target will expand as `local_test.sep.bar.c: bar.foo`.

### Special macros

The following macros have special meaning when expansion takes place
inside target definitions.

* `$@` - expands to the first output of the target.
* `$<` - expands to the first specified input of the target.
* `$^` - expands to all specified inputs of the target (not including order-only inputs).
* `$|` - expands to all order-only inputs of the target.

In addition, the following two modifiers can be appended, e.g. `$@(D)`:

* `(D)` - take only the directory name.
* `(F)` - take only the base name.

### Phony targets

Similarly to `make`, phony targets can be specified, using the pseudo `.PHONY`
output target. These are targets that don't actually generate files, but serve
only as 'always fall-through' nodes in the dependency graph, to link groups
of targets together.

```no-highlight
.PHONY: default
```

### Order-only inputs

Some target inputs can be specified in the target definition line in a way that
does not affect the expansion of `$^`.

For example:

```no-highlight
output: input | order-only
        cat $|
	    cat $^
        echo $^ > $@
```

In the target above, the execution read from `order-only`, but only the string `input`
will be written to `output`.
