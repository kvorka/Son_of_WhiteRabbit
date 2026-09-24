#pragma once
#include "../../../math/cvec.h"

extern inline __attribute__((always_inline))
void gcopy_c( const int n,
              const double *restrict gin,
                    double *restrict gtmp )

{
    
    #pragma omp unroll (vlen4) simd aligned (gin,gtmp:alignement)
    for ( int i = 0; i < n * vlen4; i++ ) { gtmp[i] = gin[i]; }
    
}