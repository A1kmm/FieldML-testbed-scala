#!/bin/bash
export API_PATH=$CMISS_ROOT/build/third_party/src/fieldml-build/
export LD_LIBRARY_PATH=$API_PATH:$API_PATH/core:$API_PATH/io:$LD_LIBRARY_PATH
export TOPDIR=$(dirname $0)

java -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n -classpath $TOPDIR/target/classes:$TOPDIR/jars/kd.jar:/usr/share/java/scala-library.jar:$API_PATH/jni/fieldml.jar -Djava.library.path=$API_PATH "$@"
