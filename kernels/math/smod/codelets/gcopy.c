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
        
        #if defined ( mem16 )
        #pragma omp unroll partial (8) simd aligned (arr_from, arr_to : 16)
        for ( int i0 = 0; i0 < 8; i0++ ) {
            
            pt[i0] = pf[i0];
            
        }
        
        pf += 8;
        pt += 8;
        
        #elif defined ( mem32 )
        #pragma omp unroll partial (16) simd aligned (arr_from, arr_to : 32)
        for ( int i0 = 0; i0 < 16; i0++ ) {
            
            pt[i0] = pf[i0];
            
        }
        
        pf += 16;
        pt += 16;
        
        #elif defined ( mem64 )
        #pragma omp unroll partial (32) simd aligned (arr_from, arr_to : 64)
        for ( int i0 = 0; i0 < 32; i0++ ) {
            
            pt[i0] = pf[i0];
            
        }
        
        pf += 32;
        pt += 32;
        #endif
        
    }
    
}