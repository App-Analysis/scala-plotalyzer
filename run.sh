#!/bin/bash

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname $SCRIPT_ABS_PATH)

JAVA_OPTS="-Xmx25g  -Xss100m --add-opens=java.base/java.util.jar=ALL-UNNAMED" $SCRIPT_ABS_DIR/target/universal/stage/bin/plotalyzer -- $@
