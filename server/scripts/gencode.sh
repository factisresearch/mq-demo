#!/bin/bash

HERE=$(cd $(dirname ${BASH_SOURCE[0]}) > /dev/null && pwd -P)
cd "$HERE"/../.. > /dev/null

TRINITY=server/build/Trinity_f
PROTOC=protoc
HPROTOC=hprotoc

if [ "$1" = "--help" -o "$1" = "-h" -o -z "$1" -o -z "$2" -o -z "$3" -o -z "$4" -o -z "$5" ]
then
    echo -n "USAGE: $0 [--verbose | --debug] PROTO_DIR SERVER_PROTO_DIR CPX_PROTO_DIR "
    echo "GEN_HS_DIR GEN_JS_DIR --protos PROTO... --server-protos PROTO..."
    exit 1
fi

if [ "$1" == "--verbose" ]
then
    LOGLEVEL=NOTE
    redir="/dev/stdout"
    debug_redir="/dev/null"
    shift
elif [ "$1" == "--debug" ]
then
    LOGLEVEL=INFO
    redir="/dev/stdout"
    debug_redir="/dev/stdout"
    shift
else
    LOGLEVEL=WARN
    redir="/dev/null"
    debug_redir="/dev/null"
fi

MODE="$1"
shift
PROTO_DIR="server/$1"
shift
GEN_HS_DIR="server/$1"
shift

PROTOS=""
while [ ! -z "$1" ]
do
    if [ "$1" == "--protos" ]
    then
        proc_protos=yes
        proc_server_protos=no
        proc_cpx_protos=no
    elif [ "$proc_protos" == "yes" ]
    then
        PROTOS="$PROTOS server/$1"
    fi
    shift
done

echo "PROTO_DIR: $PROTO_DIR" > $debug_redir
echo "GEN_HS_DIR: $GEN_HS_DIR" > $debug_redir
echo "PROTOS: $PROTOS" > $debug_redir

if [ "$MODE" == "hprotoc" ]
then
    echo "Generating Haskell code with hprotoc" > $redir

    for x in $PROTOS $SERVER_PROTOS $CPX_PROTOS
    do
        echo "processing $x" > $debug_redir
        $HPROTOC -v -u -d "$GEN_HS_DIR" \
            -I $PROTO_DIR \
            -I $SERVER_PROTO_DIR \
            -I $CPX_PROTO_DIR \
            "$x" > $debug_redir || exit 1
    done

    # Fix language pragma
    tmp=$(mktemp proto.XXXXXXXXXX)
    for x in $(find "$GEN_HS_DIR"/Com -name '*.hs' -o -name '*.hs-boot')
    do
        echo "Adding LANGUAGE pragma to $x" > $debug_redir
        echo -e "{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}\n"\
"{-# OPTIONS_GHC -fno-warn-unused-imports #-}" | \
            cat - "$x" > "$tmp" && mv "$tmp" "$x"
    done

else
    echo "Unknown mode: $MODE"
    exit 1
fi
