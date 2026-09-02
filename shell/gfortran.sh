#!/bin/bash
###########################################################################################
####                                  GFORTRAN SET UP                                  ####
###########################################################################################
fcompile="gfortran-12 -Ofast \
                      -march=native \
                      -mno-vzeroupper \
                      -mprefer-vector-width=512 \
                      -finline-functions \
                      -fno-bounds-check \
                      -fno-tree-loop-distribute-patterns \
                      -fcx-limited-range \
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
                      --param=large-stack-frame=10000 \
                      --param=large-stack-frame-growth=2000 \
                      -cpp"

ccompile="gcc-12 -Ofast \
                 -march=native \
                 -mno-vzeroupper \
                 -fno-bounds-check \
                 -fno-tree-loop-distribute-patterns \
                 -fstrict-aliasing \
                 -fomit-frame-pointer \
                 -fno-stack-protector \
                 -flto=auto \
                 -fwhole-program \
                 -fopenmp \
                 -D$memory \
                 -D$instructions \
                 -cpp"