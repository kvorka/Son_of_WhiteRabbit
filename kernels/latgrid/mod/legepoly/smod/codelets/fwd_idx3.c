#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void fwd_idx3_c( const int length,
                 const double *restrict rcab,
                       double complex *restrict cab )

{
    
    // Casting memory references
    const double *restrict prcab = rcab + 2*length;
          double *restrict pcab  = ( double * ) cab;
    
    // Main loop
    #pragma omp unroll partial (16) simd
    for ( int i = 0; i < 2*length; i++ ) {
        
        pcab[i] = prcab[i];
        
    }
    
}