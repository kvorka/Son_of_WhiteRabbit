#include <immintrin.h>

extern inline __attribute__((always_inline))
void tempcpy_c( const int n,
                const double *restrict arr_from,
                      double *restrict arr_to )

#if defined ( mem32 )
{
    
    // Memory addresses
    const double *pf = arr_from;
          double *pt = arr_to;
    
    // Registers to be used
    __m256d r0, r1, r2, r3;
    
    for ( int i2 = 0; i2 < n; i2++ ) {
        
        r0 = _mm256_load_pd( pf +  0 );
        r1 = _mm256_load_pd( pf +  4 );
        r2 = _mm256_load_pd( pf +  8 );
        r3 = _mm256_load_pd( pf + 12 );
        
        _mm256_store_pd( pt +  0, r0 );
        _mm256_store_pd( pt +  4, r1 );
        _mm256_store_pd( pt +  8, r2 );
        _mm256_store_pd( pt + 12, r3 );
        
        // Walking towards next i2 iteration
        pf += 16;
        pt += 16;
        
    }
    
}
#else
{
    
    // Memory addresses
    const double *pf = arr_from;
          double *pt = arr_to;
    
    // Registers to be used
    __m512d r0, r1, r2, r3;
    
    for ( int i2 = 0; i2 < n; i2++ ) {
        
        r0 = _mm512_load_pd( pf +  0 );
        r1 = _mm512_load_pd( pf +  8 );
        r2 = _mm512_load_pd( pf + 16 );
        r3 = _mm512_load_pd( pf + 24 );
        
        _mm512_store_pd( pt +  0, r0 );
        _mm512_store_pd( pt +  8, r1 );
        _mm512_store_pd( pt + 16, r2 );
        _mm512_store_pd( pt + 24, r3 );
        
        // Walking towards next i2 iteration
        pf += 32;
        pt += 32;
        
    }
    
}
#endif