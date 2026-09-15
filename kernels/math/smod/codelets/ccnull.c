#include "../../cvec.h"

extern inline __attribute__((always_inline))
void zero_carray_c( const int length,
                          double complex *restrict arr )

{
    
    // Casting memory addresses
    double *restrict parr = ( double * ) arr;
    
    // Main loop
    #pragma omp unroll partial (4*vlen) simd
    for ( int i = 0; i < 2*length; i++ ) { parr[i] = 0.; }
    
}