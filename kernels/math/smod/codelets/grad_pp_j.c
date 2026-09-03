#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void grad_pp_j_c( const int length,
                  const double fac1,
                  const double fac2,
                  const double fac3,
                  const double fac4,
                  const double complex *restrict darr,
                  const double complex *restrict arr,
                        double complex *restrict grad1,
                        double complex *restrict grad3 )

{
    
    // Casting memory addresses
    const double *restrict pdarr = ( const double * ) darr;
    const double *restrict parr  = ( const double * ) arr;
          double *restrict pg1   = (       double * ) grad1;
          double *restrict pg3   = (       double * ) grad3;
    
    // Main loop
    #if defined ( mem16 )
    #pragma omp unroll partial (4) simd uniform (fac1,fac2,fac3,fac4)
    #elif defined ( mem32 )
    #pragma omp unroll partial (8) simd uniform (fac1,fac2,fac3,fac4)
    #elif defined ( mem64 )
    #pragma omp unroll partial (16) simd uniform (fac1,fac2,fac3,fac4)
    #endif
    for ( int i = 0; i < 2*length; i++ ) {
        
        pg1[i] = fac1 * ( pdarr[i] + fac2 * parr[i] );
        pg3[i] = fac3 * ( pdarr[i] + fac4 * parr[i] );
        
    }
    
}