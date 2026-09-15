#include "../../cvec.h"

extern inline __attribute__((always_inline))
void zero_rarray_c( const int istart,
                    const int length,
                          double *restrict arr )

{
    
    // Starting memory address
    double *restrict parr = arr + istart;
    
    // Main loop
    #pragma omp unroll partial (4*vlen) simd
    for ( int i = 0; i < length-istart; i++ ) { parr[i] = 0.; }
    
}