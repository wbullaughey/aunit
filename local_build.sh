#!/bin/sh
echo running $0 at `pwd`
export TOOL=$1
shift

export PROGRAM=aunit
echo build $PROGRAM for ARCHITECTURE $ARCHITECTURE TARGET $TARGET using $TOOL
echo building libraries with $LIB
export BUILD_DATE=`date`
# echo BUILD_DATE=$BUILD_DATE
export PROJECT_DIRECTORY="."
echo PROJECT_DIRECTORY=$PROJECT_DIRECTORY
export GPR_PROJECT_PATH="$PROJECT_DIRECTORY/gpr"
echo GPR_PROJECT_PATH=$GPR_PROJECT_PATH
echo "TARGET=$TARGET"
export SVN_DIRECTORY=$ROOT_DIRECTORY
echo "SVN_DIRECTORY=$SVN_DIRECTORY"

echo EXTRA="$EXTRA"
export USER=$USER
echo USER=$USER

$GPR_PROJECT_PATH/build_base.sh $TOOL $PROGRAM $ARCHITECTURE $TARGET $PROJECT_DIRECTORY $EXTRA \
-aP.                                \
-aP../../project/ada_lib         \
-aP..       \
-aP$ARCHITECTURE       \
-aP$GPR_PROJECT_PATH             \


