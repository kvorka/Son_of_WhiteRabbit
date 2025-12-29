#!/bin/bash
###########################################################################################
####                                    COMPILE SHTNS                                  ####
###########################################################################################
if [ $1 = 1 ]; then
    cd shtns/
    ./configure --enable-avx --disable-openmp --enable-march=native --prefix=$(pwd)
    make
    make install
    cd ..
fi
