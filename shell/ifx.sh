#!/bin/bash
###########################################################################################
####                                     IFX SET UP                                    ####
###########################################################################################
fcompile="ifx -O3 \
              -ipo \
              -xHost \
              -fp-model fast=2 \
              -funroll-loops \
              -fomit-frame-pointer \
              -fno-stack-protector \
              -qopt-zmm-usage=high \
              -qopenmp \
              -D$memory \
              -D$code_type \
              -cpp"

ccompile="icx -O3 \
              -ipo \
              -xHost \
              -fp-model=2 \
              -fno-alias \
              -fomit-frame-pointer \
              -fno-stack-protector \
              -qopenmp-simd \
              -D$memory \
              -std=c11"