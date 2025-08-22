#!/bin/bash

# Copyright (C) 2013-2019 Ole Tange and Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

  
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && cd -P "$( dirname "$SOURCE" )" && /bin/pwd )"

if type -a $thisdir/parallel 2>&1 >/dev/null; then
  echo Parallel already installed, exiting happily.
  exit 0
else
  echo "Parallel not found; installing now."
fi

    if ! perl -e 1; then
	echo No perl installed. GNU Parallel depends on perl. Install perl and retry.
	exit 1
    fi

    LANG=C
    LATEST=parallel-20200122
    if test \! -e $LATEST.tar.bz2; then
	echo No $LATEST.tar.bz2 found, please cd to the right location or download copy
    fi
	# deleted signature checks

    bzip2 -dc $LATEST.tar.bz2 | tar xf -
    cd $LATEST || exit 2
#	if ./configure --prefix=$thisdir && make && make install; then
#  	    echo
#	    echo GNU $LATEST installed in $thisdir
#	else
       	    chmod 755 src/*;
	    mv src/parallel src/env_parallel* src/sem src/sql src/niceload src/parcat $thisdir;
	    echo
       	    echo GNU $LATEST copied to $thisdir
	#fi


#	# Is $HOME/share/man already in $MANPATH?
#	if echo $MANPATH | grep $HOME/share/man >/dev/null; then
#	    # $HOME/share/man is already in $MANPATH
#	    true
#	else
#	    # Add $HOME/share/man to $MANPATH for both bash and csh
#	    echo 'MANPATH=$MANPATH:$HOME/share/man' >> $HOME/.bashrc
#	    echo 'setenv MANPATH ${MANPATH}:${HOME}/share/man' >> $HOME/.cshrc
#	fi
    #fi
