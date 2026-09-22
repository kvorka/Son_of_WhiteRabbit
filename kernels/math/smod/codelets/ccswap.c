#include "../../cvec.h"

extern inline __attribute__((always_inline))
void swap_carray_c( const int length,
                          double complex *restrict arr1,
                          double complex *restrict arr2 )

{
    
    // Casting memory addresses
    double *restrict p1 = ( double * ) arr1;
    double *restrict p2 = ( double * ) arr2;
    
    // Temporal store
    double temp;
    
    // Main loop
    #pragma omp unroll partial (vlen4) simd
    for ( int i = 0; i < 2*length; i++ ) {
        
        temp  = p1[i];
        p1[i] = p2[i];
        p2[i] = temp;
        
    }
    
}