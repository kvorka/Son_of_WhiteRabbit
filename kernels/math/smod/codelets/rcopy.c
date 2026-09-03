#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void copy_rarray_c( const int istart,
                    const int length,
                    const double *restrict arr_from,
                          double *restrict arr_to )

{
    
    // Starting memory addresses
    const double *restrict pf = arr_from + istart -1;
          double *restrict pt = arr_to;
    
    // Main loop
    #if defined ( mem16 )
    #pragma omp unroll partial (8) simd
    #elif defined ( mem32 )
    #pragma omp unroll partial (16) simd
    #elif defined ( mem64 )
    #pragma omp unroll partial (32) simd
    #endif
    for ( int i = 0; i < length; i++ ) {
        
        pt[i] = pf[i];
        
    }
    
}