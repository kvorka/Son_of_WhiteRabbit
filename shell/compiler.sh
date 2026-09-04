#!/bin/bash
###########################################################################################
####                                 COMPILER SET UP                                   ####
###########################################################################################
case $compiler in
    ifx)
        source ./shell/ifx.sh
    ;;
    
    gfortran)
        source ./shell/gfortran.sh
    ;;
esac

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