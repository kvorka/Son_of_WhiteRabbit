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
function compile_lvl() {
    
    local -n dirs=$2
    
    if [ "$1" == "C" ]
        then        
            for dir in "${dirs[@]}"
                do
                    find "$dir" -maxdepth 1 -name "*.c" -exec $ccompile -c {} + &
                done
        else
            for dir in "${dirs[@]}"
                do
                    find "$dir" -maxdepth 1 -name "*.f90" -exec $fcompile -c {} + &
                done
    fi
    
    wait
    
}

###########################################################################################
####                                  CLEANING FUNCTION                                ####
###########################################################################################
function libclean() {
    rm *.smod *.mod *.o || true
}