#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void bwd_idx2_c( const int length,
                 const double *restrict cff,
                 const double complex *restrict cab,
                       double *restrict rcab )

{
    
    // Casting memory references
    const double *restrict pcab1 = ( const double * ) ( cab + 0*length );
    const double *restrict pcab3 = ( const double * ) ( cab + 2*length );
    
    // Constants
    const double fac1 = *( cff + 0 );
    const double fac2 = *( cff + 1 );
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac1,fac2)
    for ( int i = 0; i < 2*length; i++ ) {
        
        rcab[i] = fac1 * pcab1[i] + fac2 * pcab3[i];
        
    }
    
}