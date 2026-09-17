#include "../../../math/cvec.h"

extern inline __attribute__((always_inline))
void trshf_3x3_9_carray_c( const int length,
                           const double complex *restrict v1,
                           const double complex *restrict v2,
                           const double complex *restrict v3,
                                 double complex *restrict ca )

{
    
    // Casting memory addresses
    double *restrict rca  = ( double * ) ( ca );
    
    const double *restrict v1_1 = ( const double * ) ( v1 + 0*length );
    const double *restrict v1_2 = ( const double * ) ( v1 + 1*length );
    const double *restrict v1_3 = ( const double * ) ( v1 + 2*length );
    const double *restrict v2_1 = ( const double * ) ( v2 + 0*length );
    const double *restrict v2_2 = ( const double * ) ( v2 + 1*length );
    const double *restrict v2_3 = ( const double * ) ( v2 + 2*length );
    const double *restrict v3_1 = ( const double * ) ( v3 + 0*length );
    const double *restrict v3_2 = ( const double * ) ( v3 + 1*length );
    const double *restrict v3_3 = ( const double * ) ( v3 + 2*length );
    
    // Main loop
    #pragma omp simd
    for ( int i = 0; i < length; i++ ) {
        
        rca[ 0 + 18*i] = v1_1[0 + 2*i];
        rca[ 1 + 18*i] = v1_1[1 + 2*i];
        rca[ 2 + 18*i] = v2_1[0 + 2*i];
        rca[ 3 + 18*i] = v2_1[1 + 2*i];
        rca[ 4 + 18*i] = v3_1[0 + 2*i];
        rca[ 5 + 18*i] = v3_1[1 + 2*i];
        rca[ 6 + 18*i] = v1_2[0 + 2*i];
        rca[ 7 + 18*i] = v1_2[1 + 2*i];
        rca[ 8 + 18*i] = v2_2[0 + 2*i];
        rca[ 9 + 18*i] = v2_2[1 + 2*i];
        rca[10 + 18*i] = v3_2[0 + 2*i];
        rca[11 + 18*i] = v3_2[1 + 2*i];
        rca[12 + 18*i] = v1_3[0 + 2*i];
        rca[13 + 18*i] = v1_3[1 + 2*i];
        rca[14 + 18*i] = v2_3[0 + 2*i];
        rca[15 + 18*i] = v2_3[1 + 2*i];
        rca[16 + 18*i] = v3_3[0 + 2*i];
        rca[17 + 18*i] = v3_3[1 + 2*i];
        
    }
    
}