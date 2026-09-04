#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void zero_rarray_c( const int istart,
                    const int length,
                          double *restrict arr )

{
    
    // Starting memory address
    double *restrict parr = arr + istart;
    
    // Main loop
    #if defined ( mem32 )
    #pragma omp unroll partial (16) simd
    #elif defined ( mem64 )
    #pragma omp unroll partial (32) simd
    #endif
    for ( int i = 0; i < length-istart; i++ ) {
        
        parr[i] = 0.;
        
    }
    
}