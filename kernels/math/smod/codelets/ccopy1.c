#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void copy1_carray_c( const int length,
                     const double fac,
                           double complex *restrict arr )

{
    
    // Casting memory addresses
    double *restrict parr = ( double * ) arr;
    
    // Main loop
    #if defined ( mem16 )
    #pragma omp unroll partial (8) simd uniform (fac)
    #elif defined ( mem32 )
    #pragma omp unroll partial (16) simd uniform (fac)
    #elif defined ( mem64 )
    #pragma omp unroll partial (32) simd uniform (fac)
    #endif
    for ( int i = 0; i < 2*length; i++ ) {
        
        parr[i] = fac * parr[i];
        
    }
    
}