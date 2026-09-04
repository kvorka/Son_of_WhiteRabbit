#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void bwd_idx1_c( const int length,
                      const double fac,
                      const double complex *restrict cab,
                            double *restrict rcab )

{
    
    // Casting memory references
    const double *restrict pcab = ( const double * ) cab;
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac)
    for ( int i = 0; i < 2*length; i++ ) {
        
        rcab[i] = fac * pcab[i];
        
    }
    
}