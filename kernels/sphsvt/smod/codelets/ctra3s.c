#include "../../../math/cvec.h"

extern inline __attribute__((always_inline))
void trshf_3_3_carray_c( const int length,
                         const double complex *restrict arr_from,
                               double complex *restrict arr_to )

{
    
    // Casting memory addresses
    double *restrict rto = ( double * ) ( arr_to );
    
    const double *restrict rfrom1 = ( const double * ) ( arr_from + 0*length );
    const double *restrict rfrom2 = ( const double * ) ( arr_from + 1*length );
    const double *restrict rfrom3 = ( const double * ) ( arr_from + 2*length );
    
    // Main loop
    #pragma omp simd
    for ( int i = 0; i < length; i++ ) {
        
        rto[0 + 6*i] = rfrom1[0+2*i];
        rto[1 + 6*i] = rfrom1[1+2*i];
        rto[2 + 6*i] = rfrom3[0+2*i];
        rto[3 + 6*i] = rfrom3[1+2*i];
        rto[4 + 6*i] = rfrom2[0+2*i];
        rto[5 + 6*i] = rfrom2[1+2*i];
        
    }
    
}