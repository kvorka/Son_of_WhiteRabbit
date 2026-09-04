#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void zero_carray_c( const int length,
                          double complex *restrict arr )

{
    
    // Casting memory addresses
    double *restrict parr = ( double * ) arr;
    
    // Main loop
    #if defined ( mem32 )
    #pragma omp unroll partial (16) simd
    #elif defined ( mem64 )
    #pragma omp unroll partial (32) simd
    #endif
    for ( int i = 0; i < 2*length; i++ ) {
        
        parr[i] = 0.;
        
    }
    
}