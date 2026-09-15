#include "../../cvec.h"

extern inline __attribute__((always_inline))
void copy5_carray_c( const int length,
                     const double fac1,
                     const double fac2,
                     const double fac3,
                     const double fac4,
                     const double complex *restrict arr1,
                     const double complex *restrict arr2,
                     const double complex *restrict arr3,
                           double complex *restrict arr_to )

{
    
    // Casting memory addresses
    const double *restrict p1 = ( const double * ) arr1;
    const double *restrict p2 = ( const double * ) arr2;
    const double *restrict p3 = ( const double * ) arr3;
          double *restrict pt = (       double * ) arr_to;
    
    // Main loop
    #pragma omp unroll partial (2*vlen) simd uniform (fac1,fac2,fac3,fac4)
    for ( int i = 0; i < 2*length; i++ ) { pt[i] = fac1 * p1[i] + fac2 * p2[i] + fac3 * p3[i] + fac4 * pt[i]; }
    
}