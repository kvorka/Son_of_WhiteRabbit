#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxrsc_c( const int m,
              const double *restrict fac,
                    double *restrict arr )

#if defined ( mem32 )
{
    
    // Memory addresses
    double *parr = arr;
    
    // Registers to be used
    const __m256d rfac = _mm256_broadcast_sd( fac );
    
    // Other registers to be used
    __m256d r00, r01, r02, r03;
    
    for ( int i2 = 0; i2 < m; i2++ ) {
        
        r00 = _mm256_load_pd( parr +  0 );
        r01 = _mm256_load_pd( parr +  4 );
        r02 = _mm256_load_pd( parr +  8 );
        r03 = _mm256_load_pd( parr + 12 );
        
        r00 = _mm256_mul_pd( rfac, r00 );
        r01 = _mm256_mul_pd( rfac, r01 );
        r02 = _mm256_mul_pd( rfac, r02 );
        r03 = _mm256_mul_pd( rfac, r03 );
        
        _mm256_store_pd( parr +  0, r00 );
        _mm256_store_pd( parr +  4, r01 );
        _mm256_store_pd( parr +  8, r02 );
        _mm256_store_pd( parr + 12, r03 );
        
        // Walking towards next i2 iteration
        parr += 16;
        
    }
    
}
#else
{
    
    // Memory addresses
    double *parr = arr;
    
    // Registers to be used
    const __m512d rfac = _mm512_set1_pd( *fac );
    
    // Other registers to be used
    __m512d r00, r01, r02, r03;
    
    for ( int i2 = 0; i2 < m; i2++ ) {
        
        r00 = _mm512_load_pd( parr +  0 );
        r01 = _mm512_load_pd( parr +  8 );
        r02 = _mm512_load_pd( parr + 16 );
        r03 = _mm512_load_pd( parr + 24 );
        
        r00 = _mm512_mul_pd( rfac, r00 );
        r01 = _mm512_mul_pd( rfac, r01 );
        r02 = _mm512_mul_pd( rfac, r02 );
        r03 = _mm512_mul_pd( rfac, r03 );
        
        _mm512_store_pd( parr +  0, r00 );
        _mm512_store_pd( parr +  8, r01 );
        _mm512_store_pd( parr + 16, r02 );
        _mm512_store_pd( parr + 24, r03 );
        
        // Walking towards next i2 iteration
        parr += 32;
        
    }
    
}
#endif