#!/bin/bash
###########################################################################################
####                                     FLAG CHECK                                    ####
###########################################################################################
#code type
INSTRUCTIONS="-D$code_type"

#AVX-512F
if gcc-12 -march=native -dM -E -x c /dev/null | grep -q "__AVX512F__"
    then
        INSTRUCTIONS="-D__AVX512F__"
fi

#FMA
if gcc-12 -march=native -dM -E -x c /dev/null | grep -q "__FMA__"
    then
        INSTRUCTIONS="$INSTRUCTIONS -D__FMA__"
fi

###########################################################################################
####                                  GFORTRAN SET UP                                  ####
###########################################################################################
fcompile="gfortran-12 \
                -Ofast \
                -march=native \
                -mprefer-vector-width=512 \
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
                -cpp \
                $INSTRUCTIONS"

ccompile="gcc-12 \
                -Ofast \
                -march=native \
                -mprefer-vector-width=512 \
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
                -x c \
                $INSTRUCTIONS"

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