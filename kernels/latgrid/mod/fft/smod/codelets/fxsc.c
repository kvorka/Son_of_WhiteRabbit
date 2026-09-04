extern inline __attribute__((always_inline))
void fxrsc_c( const int m,
              const double fac,
                    double *restrict arr )

{
    
    // Main loop
    #if defined ( mem32 )
    
    #pragma omp unroll (16) simd uniform (fac) aligned (arr:32)
    for ( int i = 0; i < 16*m; i++ ) {
        
        arr[i] = fac * arr[i];
    }
    
    #elif defined ( mem64 )
    
    #pragma omp unroll (32) simd uniform (fac) aligned (arr:64)
    for ( int i = 0; i < 32*m; i++ ) {
        
        arr[i] = fac * arr[i];
    }
    
    #endif
    
}