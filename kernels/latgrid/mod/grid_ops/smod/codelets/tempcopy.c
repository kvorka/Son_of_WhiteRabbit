#include <immintrin.h>

extern inline __attribute__((always_inline))
void tempcpy_c( const int n,
                const double *restrict arr_from,
                      double *restrict arr_to )

#if defined ( mem32 )
{
    
    // Memory addresses
    const double *pfrom0 = arr_from +  0;
    const double *pfrom1 = arr_from +  4;
    const double *pfrom2 = arr_from +  8;
    const double *pfrom3 = arr_from + 12;
    
    double *pto0 = arr_to +  0;
    double *pto1 = arr_to +  4;
    double *pto2 = arr_to +  8;
    double *pto3 = arr_to + 12;
    
    // Registers to be used
    __m256d r0, r1, r2, r3;
    
    for ( int i2 = 0; i2 < n; i2++ ) {
        
        r0 = _mm256_load_pd( pfrom0 );
        r1 = _mm256_load_pd( pfrom1 );
        r2 = _mm256_load_pd( pfrom2 );
        r3 = _mm256_load_pd( pfrom3 );
        
        _mm256_store_pd( pto0, r0 );
        _mm256_store_pd( pto1, r1 );
        _mm256_store_pd( pto2, r2 );
        _mm256_store_pd( pto3, r3 );
        
        // Walking towards next i2 iteration
        pfrom0 += 16;
        pfrom1 += 16;
        pfrom2 += 16;
        pfrom3 += 16;
        
        pto0 += 16;
        pto1 += 16;
        pto2 += 16;
        pto3 += 16;
        
    }
    
}
#else
{
    
    // Memory addresses
    const double *pfrom0 = arr_from +  0;
    const double *pfrom1 = arr_from +  8;
    const double *pfrom2 = arr_from + 16;
    const double *pfrom3 = arr_from + 24;
    
    double *pto0 = arr_to +  0;
    double *pto1 = arr_to +  8;
    double *pto2 = arr_to + 16;
    double *pto3 = arr_to + 24;
    
    // Registers to be used
    __m512d r0, r1, r2, r3;
    
    for ( int i2 = 0; i2 < n; i2++ ) {
        
        r0 = _mm512_load_pd( pfrom0 );
        r1 = _mm512_load_pd( pfrom1 );
        r2 = _mm512_load_pd( pfrom2 );
        r3 = _mm512_load_pd( pfrom3 );
        
        _mm512_store_pd( pto0, r0 );
        _mm512_store_pd( pto1, r1 );
        _mm512_store_pd( pto2, r2 );
        _mm512_store_pd( pto3, r3 );
        
        // Walking towards next i2 iteration
        pfrom0 += 32;
        pfrom1 += 32;
        pfrom2 += 32;
        pfrom3 += 32;
        
        pto0 += 32;
        pto1 += 32;
        pto2 += 32;
        pto3 += 32;
        
    }
    
}
#endif