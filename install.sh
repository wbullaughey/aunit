#!/bin/sh
# $1 = architecture: intel | macosx | ppm | lightender
# $2 = target: debug | release
echo running $0 at `pwd`

export ARCHITECTURE=$1
export TARGET=$2
shift 2
export LIB=static

#export OPTIONS="-v -f"

while [ "$1" != "" ]; do    # use this to add extra defines like EDFA1
    echo define $1
        export $1
        shift 1
done

#export GPR_OPTIONS="--uninstall"
#echo GPR_OPTIONS=$GPR_OPTIONS
#./local_build.sh gprinstall

export GPR_OPTIONS=""
echo GPR_OPTIONS=$GPR_OPTIONS
./local_build.sh gprinstall
