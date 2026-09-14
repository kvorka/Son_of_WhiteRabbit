#!/bin/bash
###########################################################################################
####                                  GFORTRAN SET UP                                  ####
###########################################################################################
fcompile="gfortran-12 \
                -Ofast \
                -march=native \
                -mno-vzeroupper \
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
                -D$code_type \
                -Dmem32 \
                -cpp"

ccompile="gcc-12 \
                -Ofast \
                -march=native \
                -mno-vzeroupper \
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
                -Dmem32 \
                -cpp"

###########################################################################################
####                                COMPILE DIR STRUCTURE                              ####
###########################################################################################
function fcompile_lvl() {
    
    local -n dirs=$1
    
    for dir in "${dirs[@]}"
        do
            find "$dir" -maxdepth 1 -name "*.f90" -exec $fcompile -c {} + &
        done
    
}

function ccompile_lvl() {
    
    local -n dirs=$1
    
    for dir in "${dirs[@]}"
        do
            find "$dir" -maxdepth 1 -name "*.c" -exec $ccompile -c {} + &
        done
    
}

###########################################################################################
####                                  CLEANING FUNCTION                                ####
###########################################################################################
function libclean() {
    rm *.smod *.mod *.o || true
}