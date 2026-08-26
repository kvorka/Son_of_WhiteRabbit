#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxcpy_c( const int m,
              const double *restrict arr_from,
                    double *restrict arr_to )

#if defined ( mem32 )
{
    
    // Memory addresses
    const double *pfrom = arr_from;
          double *pto   = arr_to;
    
    // Registers to be used
    __m256d r0, r1, r2, r3,
            r4, r5, r6, r7;
    
    for ( int i2 = 0; i2 < m; i2++ ) {
        
        r0 = _mm256_load_pd( pfrom +  0 );
        r1 = _mm256_load_pd( pfrom +  4 );
        r2 = _mm256_load_pd( pfrom +  8 );
        r3 = _mm256_load_pd( pfrom + 12 );
        r4 = _mm256_load_pd( pfrom + 16 );
        r5 = _mm256_load_pd( pfrom + 20 );
        r6 = _mm256_load_pd( pfrom + 24 );
        r7 = _mm256_load_pd( pfrom + 28 );
        
        _mm256_store_pd( pto +  0, r0 );
        _mm256_store_pd( pto +  4, r1 );
        _mm256_store_pd( pto +  8, r2 );
        _mm256_store_pd( pto + 12, r3 );
        _mm256_store_pd( pto + 16, r4 );
        _mm256_store_pd( pto + 20, r5 );
        _mm256_store_pd( pto + 24, r6 );
        _mm256_store_pd( pto + 28, r7 );
        
        // Walking towards next i2 iteration
        pfrom += 32;
        pto   += 32;
        
    }
    
}
#else
{
    
    // Memory addresses
    const double *pfrom = arr_from;
          double *pto   = arr_to;
    
    // Registers to be used
    __m512d r0, r1, r2, r3,
            r4, r5, r6, r7;
    
    for ( int i2 = 0; i2 < m; i2++ ) {
        
        r0 = _mm512_load_pd( pfrom +  0 );
        r1 = _mm512_load_pd( pfrom +  8 );
        r2 = _mm512_load_pd( pfrom + 16 );
        r3 = _mm512_load_pd( pfrom + 24 );
        r4 = _mm512_load_pd( pfrom + 32 );
        r5 = _mm512_load_pd( pfrom + 40 );
        r6 = _mm512_load_pd( pfrom + 48 );
        r7 = _mm512_load_pd( pfrom + 56 );
        
        _mm512_store_pd( pto +  0, r0 );
        _mm512_store_pd( pto +  8, r1 );
        _mm512_store_pd( pto + 16, r2 );
        _mm512_store_pd( pto + 24, r3 );
        _mm512_store_pd( pto + 32, r4 );
        _mm512_store_pd( pto + 40, r5 );
        _mm512_store_pd( pto + 48, r6 );
        _mm512_store_pd( pto + 56, r7 );
        
        // Walking towards next i2 iteration
        pfrom += 64;
        pto   += 64;
        
    }
    
}
#endif