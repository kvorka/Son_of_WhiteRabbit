#include "../../../math/cvec.h"

extern inline __attribute__((always_inline))
void gcopy_c( const int n,
              const double *restrict arr_from,
                    double *restrict arr_to )

{
    
    // Memory references to be used
    const double *restrict pf = arr_from;
          double *restrict pt = arr_to;
    
    // Main cycle
    for ( int i1 = 0; i1 < n; i1++ ) {
        
        #pragma omp unroll (4*vlen) simd aligned (arr_from, arr_to : alignement)
        for ( int i0 = 0; i0 < 4*vlen; i0++ ) { pt[i0] = pf[i0]; }
        
        pf += 4*vlen;
        pt += 4*vlen;
        
    }
    
}