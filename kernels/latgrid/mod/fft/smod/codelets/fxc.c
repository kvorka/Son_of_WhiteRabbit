#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxc2r_c( const int m,
              const double *restrict t,
                    double *restrict x11,
                    double *restrict x12,
                    double *restrict x21,
                    double *restrict x22 )

#if defined ( mem32 )
{
    
    // Memory addresses
    double *px11 = x11;
    double *px12 = x12;
    double *px21 = x21;
    double *px22 = x22;
    
    // Register constants
    const __m256d rTwo = _mm256_set1_pd( 2.0 );
    const __m256d rt1  = _mm256_broadcast_sd( t + 0 );
    const __m256d rt2  = _mm256_broadcast_sd( t + 1 );
    
    // Registers
    __m256d r01, r02, r03, r04, r05, r06;
    
    for ( int i2 = 0; i2 < m; i2++ ) {
        
        for ( int i1 = 0; i1 < 4; i1++ ) {
            
            r01 = _mm256_load_pd( px11 );
            r02 = _mm256_load_pd( px21 );
            
            r03 = _mm256_add_pd( r01, r02 );
            r04 = _mm256_sub_pd( r01, r02 );
            
            r01 = _mm256_load_pd( px12 );
            r02 = _mm256_load_pd( px22 );
            
            r05 = _mm256_add_pd( r01, r02 );
            r06 = _mm256_sub_pd( r01, r02 );
            
            #if defined (__FMA__)
            r01 = _mm256_fnmadd_pd( rt2, r04, r03 );
            r02 = _mm256_fnmadd_pd( rt2, r05, r06 );
            
            r01 = _mm256_fnmadd_pd( rt1, r05, r01 );
            r02 = _mm256_fmadd_pd(  rt1, r04, r02 );
            #else
            r01 = _mm256_mul_pd( rt2, r04 );
            r02 = _mm256_mul_pd( rt2, r05 );
            
            r01 = _mm256_sub_pd( r03, r01 );
            r02 = _mm256_sub_pd( r06, r02 );
            
            r05 = _mm256_mul_pd( rt1, r05 );
            r04 = _mm256_mul_pd( rt1, r04 );
            
            r01 = _mm256_sub_pd( r01, r05 );
            r02 = _mm256_add_pd( r02, r04 );
            #endif
            
            _mm256_store_pd( px11, r01 );
            _mm256_store_pd( px12, r02 );
            
            #if defined (__FMA__)
            r01 = _mm256_fmsub_pd(  rTwo, r03, r01 );
            r02 = _mm256_fnmadd_pd( rTwo, r06, r02 );
            #else
            r03 = _mm256_add_pd( r03, r03 );
            r06 = _mm256_add_pd( r06, r06 );
            
            r01 = _mm256_sub_pd( r03, r01 );
            r02 = _mm256_sub_pd( r02, r06 );
            #endif
            
            _mm256_store_pd( px21, r01 );
            _mm256_store_pd( px22, r02 );
            
            // Walking to the next simd line
            px11 += 4;
            px12 += 4;
            px21 += 4;
            px22 += 4;
            
        }
        
    }
    
}
#else
{
    
    // Memory addresses
    double *px11 = x11;
    double *px12 = x12;
    double *px21 = x21;
    double *px22 = x22;
    
    // Register constants
    const __m512d rTwo = _mm512_set1_pd( 2.0 );
    const __m512d rt1  = _mm512_set1_pd( *( t + 0 ) );
    const __m512d rt2  = _mm512_set1_pd( *( t + 1 ) );
    
    // Registers
    __m512d r01, r02, r03, r04, r05, r06;
    
    for ( int i2 = 0; i2 < m; i2++ ) {
        
        for ( int i1 = 0; i1 < 4; i1++ ) {
            
            r01 = _mm512_load_pd( px11 );
            r02 = _mm512_load_pd( px21 );
            
            r03 = _mm512_add_pd( r01, r02 );
            r04 = _mm512_sub_pd( r01, r02 );
            
            r01 = _mm512_load_pd( px12 );
            r02 = _mm512_load_pd( px22 );
            
            r05 = _mm512_add_pd( r01, r02 );
            r06 = _mm512_sub_pd( r01, r02 );
            
            r01 = _mm512_fnmadd_pd( rt2, r04, r03 );
            r02 = _mm512_fnmadd_pd( rt2, r05, r06 );
            
            r01 = _mm512_fnmadd_pd( rt1, r05, r01 );
            r02 = _mm512_fmadd_pd(  rt1, r04, r02 );
            
            _mm512_store_pd( px11, r01 );
            _mm512_store_pd( px12, r02 );
            
            r01 = _mm512_fmsub_pd(  rTwo, r03, r01 );
            r02 = _mm512_fnmadd_pd( rTwo, r06, r02 );
            
            _mm512_store_pd( px21, r01 );
            _mm512_store_pd( px22, r02 );
            
            // Walking to the next simd line
            px11 += 8;
            px12 += 8;
            px21 += 8;
            px22 += 8;
            
        }
        
    }
    
}
#endif