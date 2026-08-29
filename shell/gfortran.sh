#!/bin/bash
###########################################################################################
####                                  GFORTRAN SET UP                                  ####
###########################################################################################
fcompile="gfortran-12 -Ofast \
                      -march=native \
                      -mno-vzeroupper \
                      -mprefer-vector-width=512 \
                      -fno-bounds-check \
                      -fargument-noalias-global \
                      -fstrict-aliasing \
                      -fomit-frame-pointer \
                      -fno-stack-protector \
                      -flto=auto \
                      -fwhole-program \
                      -fopenmp \
                      -D$memory \
                      -D$kernel \
                      -D$code_type \
                      -cpp"

ccompile="gcc-12 -Ofast \
                 -march=native \
                 -mno-vzeroupper \
                 -fno-bounds-check \
                 -fstrict-aliasing \
                 -fomit-frame-pointer \
                 -fno-stack-protector \
                 -flto=auto \
                 -fwhole-program \
                 -fopenmp \
                 -D$memory \
                 -D$instructions \
                 -cpp"