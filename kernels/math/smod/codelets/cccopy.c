#include "../../cvec.h"

extern inline __attribute__((always_inline))
void copy_carray_c( const int length,
                    const double complex *restrict arr_from,
                          double complex *restrict arr_to )

{
    
    // Casting memory addresses
    const double *restrict pf = ( const double * ) arr_from;
          double *restrict pt = (       double * ) arr_to;
    
    // Main loop
    #pragma omp unroll partial (4*vlen) simd
    for ( int i = 0; i < 2*length; i++ ) { pt[i] = pf[i]; }
    
}