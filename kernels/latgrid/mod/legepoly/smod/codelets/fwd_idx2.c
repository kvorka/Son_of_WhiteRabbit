#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void fwd_idx2_c( const int length,
                 const double *restrict cff,
                 const double *restrict rcab,
                       double complex *restrict cab )

{
    
    // Casting memory references
    const double *restrict prcab1 = rcab + 0*length;
    const double *restrict prcab3 = rcab + 4*length;
          double *restrict pcab   = ( double * ) cab;
    
    // Constants
    const double fac1 = *( cff + 0 );
    const double fac2 = *( cff + 1 );
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac1,fac2)
    for ( int i = 0; i < 2*length; i++ ) {
        
        pcab[i] = fac1 * prcab1[i] + fac2 * prcab3[i];
        
    }
    
}