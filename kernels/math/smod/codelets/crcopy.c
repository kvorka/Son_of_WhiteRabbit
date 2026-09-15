#include "../../cvec.h"

extern inline __attribute__((always_inline))
void copy_rarray_c( const int istart,
                    const int length,
                    const double *restrict arr_from,
                          double *restrict arr_to )

{
    
    // Starting memory addresses
    const double *restrict pf = arr_from + istart -1;
          double *restrict pt = arr_to;
    
    // Main loop
    #pragma omp unroll partial (4*vlen) simd
    for ( int i = 0; i < length; i++ ) { pt[i] = pf[i]; }
    
}