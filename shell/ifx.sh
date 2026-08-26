#!/bin/bash
###########################################################################################
####                                     IFX SET UP                                    ####
###########################################################################################
fcompile="ifx -O3 \
              -xHost \
              -fp-model fast=2 \
              -funroll-loops \
              -fomit-frame-pointer \
              -fno-stack-protector \
              -align array32byte \
              -qopt-zmm-usage=low \
              -qopenmp \
              -D$memory \
              -D$kernel \
              -D$code_type \
              -cpp"

ccompile="icx -O3 \
              -xHost \
              -fp-model=fast \
              -fno-alias \
              -funroll-loops \
              -fomit-frame-pointer \
              -fno-stack-protector \
              -qopt-zmm-usage=low \
              -qopenmp \
              -D$memory \
              -std=c11"