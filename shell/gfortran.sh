#!/bin/bash
###########################################################################################
####                                  GFORTRAN SET UP                                  ####
###########################################################################################
fcompile="gfortran-12 \
  -Ofast \
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
  -cpp"

ccompile="gcc-12 \
  -Ofast \
  -march=native \
  -mno-vzeroupper \
  -mprefer-vector-width=512 \
  -fcx-limited-range \
  -fno-bounds-check \
  -fno-builtin-memcpy \
  -fno-builtin-memset \
  -fno-builtin-memmove \
  -fno-tree-loop-distribute-patterns \
  -fstrict-aliasing \
  -fomit-frame-pointer \
  -fno-stack-protector \
  -flto=auto \
  -fwhole-program \
  -fopenmp-simd \
  -D$memory \
  -cpp"