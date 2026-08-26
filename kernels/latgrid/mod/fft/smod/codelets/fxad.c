#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxaddsub_c( const int m,
                       double *restrict arr1,
                       double *restrict arr2 )

#if defined ( mem32 )
{
    
    // Memory addresses
    double *parr1 = arr1; 
    double *parr2 = arr2;
    
    // Registers to be used
    __m256d r00, r01, r02, r03, 
            r04, r05, r06, r07;
    
    for ( int i2 = 0; i2 < m; i2++ ) {
        
        for ( int i1 = 0; i1 < 4; i1+=2 ) {
            
            r00 = _mm256_load_pd( parr1 + 0 );
            r01 = _mm256_load_pd( parr1 + 4 );
            
            r02 = _mm256_load_pd( parr2 + 0 );
            r03 = _mm256_load_pd( parr2 + 4 );
            
            r04 = _mm256_add_pd( r00, r02 );
            r05 = _mm256_add_pd( r01, r03 );
            
            _mm256_store_pd( parr1 + 0, r04 );
            _mm256_store_pd( parr1 + 4, r05 );
            
            r06 = _mm256_sub_pd( r00, r02 );
            r07 = _mm256_sub_pd( r01, r03 );
            
            _mm256_store_pd( parr2 + 0, r06 );
            _mm256_store_pd( parr2 + 4, r07 );
            
            // Walking to the next two simd lines
            parr1 += 8;
            parr2 += 8;
            
        }
        
    }
    
}
#else
{
    
    // Memory addresses
    double *parr1 = arr1; 
    double *parr2 = arr2;
    
    // Registers to be used
    __m512d r00, r01, r02, r03, 
            r04, r05, r06, r07;
    
    for ( int i2 = 0; i2 < m; i2++ ) {
        
        for ( int i1 = 0; i1 < 4; i1+=2 ) {
            
            r00 = _mm512_load_pd( parr1 + 0 );
            r01 = _mm512_load_pd( parr1 + 8 );
            
            r02 = _mm512_load_pd( parr2 + 0 );
            r03 = _mm512_load_pd( parr2 + 8 );
            
            r04 = _mm512_add_pd( r00, r02 );
            r05 = _mm512_add_pd( r01, r03 );
            
            _mm512_store_pd( parr1 + 0, r04 );
            _mm512_store_pd( parr1 + 8, r05 );
            
            r06 = _mm512_sub_pd( r00, r02 );
            r07 = _mm512_sub_pd( r01, r03 );
            
            _mm512_store_pd( parr2 + 0, r06 );
            _mm512_store_pd( parr2 + 8, r07 );
            
            // Walking to the next two simd lines
            parr1 += 16;
            parr2 += 16;
            
        }
        
    }
    
}
#endif