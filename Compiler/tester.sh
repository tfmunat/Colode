#!/bin/bash

for t in ../Tests/*.cld; do
  echo "Running test $t"
  ./toplevel.native < $t
  echo "-----------------------------------------"
done
