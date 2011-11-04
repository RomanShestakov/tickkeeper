#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name tickkeeper@127.0.0.1 -setcookie rs -s tk_app start
