#include "../../cvec.h"

extern inline __attribute__((always_inline))
void cadj3_carray_c( const int length,
                     const double fac,
                     const double complex *restrict arr_from,
                           double complex *restrict arr_to )

{
    
    // Casting memory addresses
    const double *restrict pf = ( const double * ) arr_from;
          double *restrict pt = (       double * ) arr_to;
    
    // Main loop
    #pragma omp unroll partial (vlen2) simd uniform (fac)
    for ( int i = 0; i < 2*length; i += 2 ) {
        
        pt[i  ] = pt[i  ] + fac * pf[i  ];
        pt[i+1] = pt[i+1] - fac * pf[i+1];
        
    }
    
}