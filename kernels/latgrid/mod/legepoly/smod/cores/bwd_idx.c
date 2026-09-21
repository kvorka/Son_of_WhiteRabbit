#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void bwd_idx0_c( const int length,
                 const double fac,
                 const double complex *restrict cab,
                       double *restrict rcab )

{
    
    // Casting memory references
    const double *restrict pcab1  = ( const double * ) ( cab + 0*length );
    const double *restrict pcab2  = ( const double * ) ( cab + 1*length );
          double *restrict prcab1 = rcab + 0*length;
          double *restrict prcab2 = rcab + 2*length;
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac)
    for ( int i = 0; i < 2*length; i++ ) {
        
        prcab1[i] = pcab2[i] * fac;
        prcab2[i] = pcab1[i];
        
    }
    
}

extern inline __attribute__((always_inline))
void bwd_idx1_c( const int length,
                 const double *restrict fac,
                 const double complex *restrict cab,
                       double *restrict rcab )

{
    
    // Casting memory references
    const double *restrict pcab1  = ( const double * ) ( cab + 0*length );
    const double *restrict pcab2  = ( const double * ) ( cab + 1*length );
    const double *restrict pcab3  = ( const double * ) ( cab + 2*length );
          double *restrict prcab1 = rcab + 0*length;
          double *restrict prcab2 = rcab + 2*length;
    
    // Constants
    const double fac1 = *( fac + 0 );
    const double fac2 = *( fac + 1 );
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac)
    for ( int i = 0; i < 2*length; i++ ) {
        
        prcab1[i] = pcab1[i] * fac1 + pcab3[i] * fac2;
        prcab2[i] = pcab2[i];
        
    }
    
}

extern inline __attribute__((always_inline))
void bwd_idx2_c( const int length,
                 const double fac,
                 const double complex *restrict cab,
                       double *restrict rcab )

{
    
    // Casting memory references
    const double *restrict pcab1  = ( const double * ) ( cab + 0*length );
    const double *restrict pcab2  = ( const double * ) ( cab + 1*length );
          double *restrict prcab1 = rcab + 0*length;
          double *restrict prcab2 = rcab + 2*length;
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac)
    for ( int i = 0; i < 2*length; i++ ) {
        
        prcab1[i] = pcab1[i] * fac;
        prcab2[i] = pcab2[i];
        
    }
    
}

extern inline __attribute__((always_inline))
void bwd_idx3_c( const int length,
                 const double fac,
                 const double complex *restrict cab,
                       double *restrict rcab )

{
    
    // Casting memory references
    const double *restrict pcab1  = ( const double * ) ( cab );
          double *restrict prcab1 = rcab + 0*length;
          double *restrict prcab2 = rcab + 2*length;
    
    // Main loop
    #pragma omp unroll partial (16) simd uniform (fac)
    for ( int i = 0; i < 2*length; i++ ) {
        
        prcab1[i] = pcab1[i] * fac;
        prcab2[i] = 0.;
        
    }
    
}

extern inline __attribute__((always_inline))
void bwd_idx4_c( const int length,
                 const double complex *restrict cab,
                       double *restrict rcab )

{
    
    // Casting memory references
    const double *restrict pcab1  = ( const double * ) ( cab );
          double *restrict prcab1 = rcab + 0*length;
          double *restrict prcab2 = rcab + 2*length;
    
    // Main loop
    #pragma omp unroll partial (16) simd
    for ( int i = 0; i < 2*length; i++ ) {
        
        prcab1[i] = 0.;
        prcab2[i] = pcab1[i];
        
    }
    
}