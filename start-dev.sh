#!/bin/sh

exec erl \
  -pa ebin \
  -pa deps/*/ebin \
  -config zview \
  -boot start_sasl \
  -s zview
