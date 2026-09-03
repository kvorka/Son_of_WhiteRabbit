#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void copy4_carray_c( const int length,
                     const double fac1,
                     const double fac2,
                     const double fac3,
                     const double complex *restrict arr1,
                     const double complex *restrict arr2,
                           double complex *restrict arr_to )

{
    
    // Casting memory addresses
    const double *restrict p1 = ( const double * ) arr1;
    const double *restrict p2 = ( const double * ) arr2;
          double *restrict pt = (       double * ) arr_to;
    
    // Main loop
    #if defined ( mem16 )
    #pragma omp unroll partial (4) simd uniform (fac1,fac2,fac3)
    #elif defined ( mem32 )
    #pragma omp unroll partial (8) simd uniform (fac1,fac2,fac3)
    #elif defined ( mem64 )
    #pragma omp unroll partial (16) simd uniform (fac1,fac2,fac3)
    #endif
    for ( int i = 0; i < 2*length; i++ ) {
        
        pt[i] = fac1 * p1[i] + fac2 * p2[i] + fac3 * pt[i];
        
    }
    
}