#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void fxaddsub_c( const int m,
                       double *restrict arr1,
                       double *restrict arr2 )

{
    
    // Temporal variables
    double add;
    
    // Main loop
    #pragma omp unroll (vlen4) simd aligned (arr1,arr2:alignement)
    for ( int i = 0; i < vlen4 * m; i++ ) {
        
        add     = arr1[i];
        arr1[i] = arr1[i] + arr2[i];
        arr2[i] = add     - arr2[i];
        
    }
    
}