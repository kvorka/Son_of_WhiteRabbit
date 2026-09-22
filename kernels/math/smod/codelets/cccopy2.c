#include "../../cvec.h"

extern inline __attribute__((always_inline))
void copy2_carray_c( const int length,
                     const double fac,
                     const double complex *restrict arr_from,
                           double complex *restrict arr_to )

{
    
    // Casting memory addresses
    const double *restrict pf = ( const double * ) arr_from;
          double *restrict pt = (       double * ) arr_to;
    
    // Main loop
    #pragma omp unroll partial (vlen4) simd uniform (fac)
    for ( int i = 0; i < 2*length; i++ ) { pt[i] = fac * pf[i]; }
    
}