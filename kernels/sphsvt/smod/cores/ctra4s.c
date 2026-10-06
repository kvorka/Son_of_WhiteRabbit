#include "../../../math/cvec.h"

extern inline __attribute__((always_inline))
void trans_4_carray_c( const int length,
                       const double complex *restrict arr_from,
                             double complex *restrict arr_to )

{
    
    // Casting memory addresses
    const double *restrict pfrom = ( const double * ) ( arr_from          );
          double *restrict pto1  = (       double * ) ( arr_to + 0*length );
          double *restrict pto2  = (       double * ) ( arr_to + 1*length );
          double *restrict pto3  = (       double * ) ( arr_to + 2*length );
          double *restrict pto4  = (       double * ) ( arr_to + 3*length );
    
    // Main loop
    #pragma GCC ivdep
    #pragma omp unroll partial (4) simd
    for ( int i = 0; i < length; i++ ) {
        
        pto1[0+2*i] = pfrom[0+8*i];
        pto1[1+2*i] = pfrom[1+8*i];
        pto2[0+2*i] = pfrom[2+8*i];
        pto2[1+2*i] = pfrom[3+8*i];
        pto3[0+2*i] = pfrom[4+8*i];
        pto3[1+2*i] = pfrom[5+8*i];
        pto4[0+2*i] = pfrom[6+8*i];
        pto4[1+2*i] = pfrom[7+8*i];
        
    }
    
}