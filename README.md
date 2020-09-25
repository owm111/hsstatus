hsstatus
========

**Unstable and underconstruction**, still deciding what direction to take.

Experimental status monitor (built for personal use). The goal is to provide
event-based monitoring for use in a status bar while remaining lightweight.

The current feature set is limited, exception handling is far from great, but
performance appears to be decent (memory use is ~5-6kB).

Installation & Building
-----------------------

Requires alsa-lib headers. Should be compiled with `-threaded` (for
concurrency) and `-with-rtsopts=-I0` (to disable idle garbage collection;
leaving it enabled leads to many unnecessary GCs and wastes CPU time).

TODO: instructions

Features
--------

-   Battery percentage and status.
-   Backlight percentage.
-   Date/time.
-   Reading and printing standard input.
-   Volume percentage and mute (without relying on /dev/mixer or alsactl).

TODO
----

-   Tests?
-   Benchmarks?
-   Documentation?
-   Make FieldTuple not so huge and ugly.
-   Exception handling.

Possible changes or ideas to consider:

-   Don't user `status` to check battery status (since it is so slow)?
-   Clean up AlsaInternal.
-   Use ByteStrings for internal IO?
-   Using channels/queues instead of variables?
    -   Revert to using a single queue of changes (one queue is then passed
        to monitors instead of a variable each)?
    -   Use a channel of pairs of IDs and new values to update an internal
        (to the printer) state?
-   Seperate pure and IO code?
    -   The Field type can be made into a record containing IO actions (e.g.
        acquire resources at the beginning, release them at the end, wait for
        new changes, etc.) and pure code to run in between?
-   Opinionate fields to output (Byte)Strings?
-   Can RTS options change/improve memory characteristics? (1MB is the
    default allocation area size, but this sounds like over kill, at least
    for now)
