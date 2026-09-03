#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void swap_carray_c( const int length,
                          double complex *restrict arr1,
                          double complex *restrict arr2 )

{
    
    // Casting memory addresses
    double *restrict p1 = ( double * ) arr1;
    double *restrict p2 = ( double * ) arr2;
    
    // Temporal store
    double temp;
    
    // Main loop
    #if defined ( mem16 )
    #pragma omp unroll partial (8) simd
    #elif defined ( mem32 )
    #pragma omp unroll partial (16) simd
    #elif defined ( mem64 )
    #pragma omp unroll partial (32) simd
    #endif
    for ( int i = 0; i < 2*length; i++ ) {
        
        temp  = p1[i];
        p1[i] = p2[i];
        p2[i] = temp;
        
    }
    
}