#!/bin/sh

if [ ! $(git rev-parse --symbolic-full-name HEAD) = refs/heads/master ]; then
    exit 0
fi

# These are the locations I keep my temporary source and build trees in
TMPDIR=$HOME/Products/ledger-pre-commit
MIRROR=$HOME/Products/ledger-pre-commit-mirror

# Exit with status 1 if any command below fails
set -e

# Checkout a copy of the current index into MIRROR
git checkout-index --prefix=$MIRROR/ -af

# Remove files from MIRROR which are no longer present in the index
git diff-index --cached --name-only --diff-filter=D -z HEAD | \
    (cd $MIRROR && xargs -0 rm -f --)

# Copy only _changed files_ from MIRROR to TMPDIR, without copying timestamps.
# This includes copying over new files, and deleting removed ones.  This way,
# "make check" will only rebuild what is necessary to validate the commit.
rsync -rlpgoDOc --delete --exclude-from=tools/excludes $MIRROR/ $TMPDIR/

# Everything else happens in the temporary build tree
if [ ! -f $TMPDIR/lib/utfcpp/source/utf8.h ]; then
    rsync -a --delete lib/utfcpp/ $TMPDIR/lib/utfcpp/
fi
cd $TMPDIR

# Make sure there is a current Makefile.  Regeneration of Makefile happens
# automatically, but if myacprep or acprep changes, we want to regenerate
# everything manually.  If the user doesn't have acprep or myacprep, look for
# other common autoconf-related script files.
if [ ! -f Makefile -o \
     Makefile.am -nt Makefile -o \
     configure.ac -nt Makefile -o \
     \( -f acprep -a acprep -nt Makefile \) -o \
     \( -f tools/myacprep -a tools/myacprep -nt Makefile \) ]
then
    if [ -f tools/myacprep ]; then
	tools/myacprep --local
    elif [ -f acprep ]; then
	./acprep --local
    elif [ -f autogen.sh ]; then
	sh autogen.sh && ./configure
    else
	autoreconf && ./configure
    fi
fi

# Finally, (re)build this proposed source tree and see if it passes muster.
nice -n 20 make check

exit 0
