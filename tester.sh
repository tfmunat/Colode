#!/bin/bash

./toplevel.native Tests/test1.cld
  echo "-----------------------------------------"
./colode.native < Tests/test2.cld
  echo "-----------------------------------------"
./colode.native < Tests/test3.cld
  echo "-----------------------------------------"
