#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void fwd_idx0_c( const int length,
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

extern inline __attribute__((always_inline))
void fwd_idx1_c( const int length,
                 const double *restrict fac,
                 const double *restrict rcab,
                       double complex *restrict cab )

{
    
    // Casting memory references
    const double *restrict prcab1 = rcab + 0*length;
    const double *restrict prcab3 = rcab + 4*length;
    const double *restrict prcab4 = rcab + 6*length;
          double *restrict pcab1  = ( double * ) ( cab + 0*length );
          double *restrict pcab2  = ( double * ) ( cab + 1*length );
    
    // Constants
    const double fac1 = *( fac + 0 );
    const double fac2 = *( fac + 1 );
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac1,fac2)
    for ( int i = 0; i < 2*length; i++ ) {
        
        pcab1[i] = prcab1[i] * fac1 + prcab3[i] * fac2;
        pcab2[i] = prcab4[i];
        
    }
    
}

extern inline __attribute__((always_inline))
void fwd_idx2_c( const int length,
                 const double *restrict fac,
                 const double *restrict rcab,
                       double complex *restrict cab )

{
    
    // Casting memory references
    const double *restrict prcab1 = rcab + 0*length;
    const double *restrict prcab3 = rcab + 4*length;
          double *restrict pcab1  = ( double * ) ( cab );
    
    // Constants
    const double fac1 = *( fac + 0 );
    const double fac2 = *( fac + 1 );
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac1,fac2)
    for ( int i = 0; i < 2*length; i++ ) {
        
        pcab1[i] = prcab1[i] * fac1 + prcab3[i] * fac2;
        
    }
    
}