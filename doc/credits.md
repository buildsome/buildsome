# Credits

Most of the code in Buildsome was written by **[Eyal Lotem](https://github.com/Peaker)**, with further contributions from the following developers:

* Noam Lewis
* Dan Aloni
* Yair Chuchem
* Ezra Hoch
* Nadav Shemer
* Renana Frim

The file system hooks are inspired by
[tup](http://gittup.org/tup/). They are taken a couple of steps
further though, by hooking the file system globally, and not just
locally, avoiding false negatives relating to system-wide changes,
too. These hooks are also used to auto-detect dependencies and delay
dependent executions, rather than just verify explicitly specified
dependencies.
