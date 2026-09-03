#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void curl_ptp_j_c( const int length,
                   const double fac1,
                   const double fac2,
                   const double fac3,
                   const double fac4,
                   const double fac5,
                   const double fac6,
                   const double complex *restrict darr1,
                   const double complex *restrict darr2,
                   const double complex *restrict darr3,
                   const double complex *restrict arr1,
                   const double complex *restrict arr2,
                   const double complex *restrict arr3,
                         double complex *restrict curl1,
                         double complex *restrict curl2,
                         double complex *restrict curl3 )

{
    
    // Casting memory addresses
    const double *restrict pdarr1 = ( const double * ) darr1;
    const double *restrict pdarr2 = ( const double * ) darr2;
    const double *restrict pdarr3 = ( const double * ) darr3;
    const double *restrict parr1  = ( const double * ) arr1;
    const double *restrict parr2  = ( const double * ) arr2;
    const double *restrict parr3  = ( const double * ) arr3;
          double *restrict pcrl1  = (       double * ) curl1;
          double *restrict pcrl2  = (       double * ) curl2;
          double *restrict pcrl3  = (       double * ) curl3;
    
    // Temporal variables
    double c1, c2, c3, c4,
           s1, s2, s3, s4,
           d1, d2, d3, d4,
           a1, a2, a3, a4;
    
    // Main loop
    #if defined ( mem16 )
    #pragma omp unroll partial (2) simd uniform(fac1,fac2,fac3,fac4,fac5,fac6)
    #elif defined ( mem32 )
    #pragma omp unroll partial (4) simd uniform(fac1,fac2,fac3,fac4,fac5,fac6)
    #elif defined ( mem64 )
    #pragma omp unroll partial (8) simd uniform(fac1,fac2,fac3,fac4,fac5,fac6)
    #endif
    for ( int i = 0; i < 2*length; i += 2 ) {
        
        a1 = parr2[0+i];
        a2 = parr2[1+i];
        
        d1 = pdarr2[0+i];
        d2 = pdarr2[1+i];
        
        c1 = fac1 * ( d1 + fac3 * a1 ); 
        c2 = fac1 * ( d2 + fac3 * a2 ); 
        c3 = fac4 * ( d1 - fac5 * a1 );
        c4 = fac4 * ( d2 - fac5 * a2 );
        
        s1 = -c2;
        s2 = +c1;
        s3 = -c4;
        s4 = +c3;
        
        pcrl1[0+i] = s1;
        pcrl1[1+i] = s2;
        
        pcrl3[0+i] = s3;
        pcrl3[1+i] = s4;
        
        a1 = parr1[0+i];
        a2 = parr1[1+i];
        
        d1 = pdarr1[0+i];
        d2 = pdarr1[1+i];
        
        c1 = fac1 * ( d1 - fac2 * a1 );
        c2 = fac1 * ( d2 - fac2 * a2 );
        
        a3 = parr3[0+i];
        a4 = parr3[1+i];
        
        d3 = pdarr3[0+i];
        d4 = pdarr3[1+i];
        
        c1 = c1 + fac4 * ( d3 + fac6 * a3 );
        c2 = c2 + fac4 * ( d4 + fac6 * a4 );
        
        s1 = -c2;
        s2 = +c1;
        
        pcrl2[0+i] = s1;
        pcrl2[1+i] = s2;
        
    }
    
}