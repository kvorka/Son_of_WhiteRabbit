#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void fxcpy_c( const int m,
              const double *restrict arr_from,
                    double *restrict arr_to )

{
    
    #pragma omp unroll (8*vlen) simd uniform (fac) aligned (arr_to,arr_from:alignement)
    for ( int i = 0; i < 8 * vlen * m; i++ ) { arr_to[i] = arr_from[i]; }
    
}