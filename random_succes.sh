#! /usr/bin/env bash

if [ "$((${RANDOM} % 5))" = "0" ]; then
  true
else
  false
fi

