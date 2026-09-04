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
    const double *restrict pd = ( const double * ) darr;
    const double *restrict pa = ( const double * ) arr;
          double *restrict p1 = (       double * ) grad1;
          double *restrict p3 = (       double * ) grad3;
    
    // Main loop
    #if defined ( mem32 )
    #pragma omp unroll partial (8) simd uniform (fac1,fac2,fac3,fac4)
    #elif defined ( mem64 )
    #pragma omp unroll partial (16) simd uniform (fac1,fac2,fac3,fac4)
    #endif
    for ( int i = 0; i < 2*length; i++ ) {
        
        p1[i] = fac1 * ( pd[i] + fac2 * pa[i] );
        p3[i] = fac3 * ( pd[i] + fac4 * pa[i] );
        
    }
    
}