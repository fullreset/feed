ACLOCAL_AMFLAGS = -I m4

LIB      := @LIB@
INCLUDE  := @INCLUDE@

LIB_UR   := $(LIB)/ur/feed

SUBDIRS  := src/c

.PHONY: package reauto

install-exec-local:
	mkdir -p $(LIB_UR)
	cp "${srcdir}"/src/ur/*.urs "$(LIB_UR)/"
	cp "${srcdir}"/src/ur/*.ur "$(LIB_UR)/"
	cp "${srcdir}"/src/ur/*.urp "$(LIB_UR)/"
	mkdir -p "$(INCLUDE)"
	cp "${srcdir}"/include/*.h "$(INCLUDE)/"
	-ldconfig

package:
	hg archive -t tgz /tmp/urweb_feed.tgz

reauto:
	-autoreconf
	libtoolize -cf
	automake --add-missing --force-missing --copy
	autoreconf
