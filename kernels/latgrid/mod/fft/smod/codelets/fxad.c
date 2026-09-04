extern inline __attribute__((always_inline))
void fxaddsub_c( const int m,
                       double *restrict arr1,
                       double *restrict arr2 )

{
    
    // Temporal variables
    double add;
    
    // Main loop
    #if defined ( mem32 )
    #pragma omp unroll (16) simd aligned (arr1,arr2:32)
    for ( int i = 0; i < 16*m; i++ ) {
        
        add     = arr1[i];
        arr1[i] = arr1[i] + arr2[i];
        arr2[i] = add     - arr2[i];
        
    }
    #else
    #pragma omp unroll (32) simd aligned (arr1,arr2:64)
    for ( int i = 0; i < 32*m; i++ ) {
        
        add     = arr1[i];
        arr1[i] = arr1[i] + arr2[i];
        arr2[i] = add     - arr2[i];
        
    }
    #endif
    
}