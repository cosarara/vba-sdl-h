VBA-SDL-H
=========

This is a fork of VBA-SDL-H, as a result of the author's repository having
been down for some time.

Vanilla VBA-SDL-H (see links at the bottom) wouldn't build on an updated arch
linux. The configure script couldn't be regenerated without bringing all that
autotools system to today's standards as well. I decided to just remove
the entire build system and write a short Makefile (which shouldn't be
too difficult to maintain).

Also, I applied these changes to fix a bug:

http://sourceforge.net/p/vba/patches/35/


Building
--------

Dependencies:

* libpng (12 or 16, you can change the makefile for each)
* SDL1.2
* zlib
* minizip
* readline

On arch linux you'll find all the dependencies in the repository.
On debian you'll have to build minizip manually. To do so (you will need
autotools):

    $ git submodule init
    $ git submodule update
    $ cd minizip
    $ rm -f Makefile
    $ autoreconf -i
    $ autoconf
    $ automake --add-missing
    $ automake
    $ ./configure --prefix=/usr
    $ make
    # make install

Config
------

The configuration file is named VisualBoyAdvance.cfg, and will be installed
in /etc/ on "make install". You can copy it to ~/.config/vba-sdl-h/", and
it will take precedence over the former.

[1] http://labmaster.bios.net.nz/vba-sdl-h/

[2] https://www.dropbox.com/s/1bp33i7j2uxhlva/vba-sdl-h-src.tar.gz?dl=0
