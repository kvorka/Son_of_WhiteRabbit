extern inline __attribute__((always_inline))
void fxcpy_c( const int m,
              const double *restrict arr_from,
                    double *restrict arr_to )

{
    
    // Main loop
    #if defined ( mem32 )
    
    #pragma omp unroll (32) simd uniform (fac) aligned (arr_to,arr_from:32)
    for ( int i = 0; i < 32*m; i++ ) {
        
        arr_to[i] = arr_from[i];
    }
    
    #elif defined ( mem64 )
    
    #pragma omp unroll (64) simd uniform (fac) aligned (arr_to,arr_from:64)
    for ( int i = 0; i < 64*m; i++ ) {
        
        arr_to[i] = arr_from[i];
    }
    
    #endif
    
}