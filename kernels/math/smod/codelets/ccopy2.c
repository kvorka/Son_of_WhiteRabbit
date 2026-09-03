#include <stddef.h>
#include <complex.h>

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
    #if defined ( mem16 )
    #pragma omp unroll partial (8) simd uniform (fac)
    #elif defined ( mem32 )
    #pragma omp unroll partial (16) simd uniform (fac)
    #elif defined ( mem64 )
    #pragma omp unroll partial (32) simd uniform (fac)
    #endif
    for ( int i = 0; i < 2*length; i++ ) {
        
        pt[i] = fac * pf[i];
        
    }
    
}