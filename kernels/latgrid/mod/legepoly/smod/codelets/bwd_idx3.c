#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void bwd_idx3_c( const int length,
                 const double complex *restrict cab,
                       double *restrict rcab )

{
    
    // Casting memory references
    const double *restrict pcab  = ( const double * ) cab;
          double *restrict prcab = rcab + 2*length;
    
    // Main loop
    #pragma omp unroll partial (16) simd
    for ( int i = 0; i < 2*length; i++ ) {
        
        prcab[i] = pcab[i];
        
    }
    
}