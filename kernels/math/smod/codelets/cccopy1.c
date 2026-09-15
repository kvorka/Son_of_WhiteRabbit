#include "../../cvec.h"

extern inline __attribute__((always_inline))
void copy1_carray_c( const int length,
                     const double fac,
                           double complex *restrict arr )

{
    
    // Casting memory addresses
    double *restrict parr = ( double * ) arr;
    
    // Main loop
    #pragma omp unroll partial (4*vlen) simd uniform (fac)
    for ( int i = 0; i < 2*length; i++ ) { parr[i] = fac * parr[i]; }
    
}