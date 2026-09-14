#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void fxrsc_c( const int m,
              const double fac,
                    double *restrict arr )

{
    
    #pragma omp unroll (4*vlen) simd uniform (fac) aligned (arr:alignement)
    for ( int i = 0; i < 4 * vlen * m; i++ ) { arr[i] = fac * arr[i]; }
    
}