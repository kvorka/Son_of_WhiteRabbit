#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void ee2xy_c( const int n,
                    double complex *restrict cx,
                    double complex *restrict cy )

#if defined ( mem32 )
{
    
    // Casting memory addresses
    double *restrict px = ( double * ) cx;
    double *restrict py = ( double * ) cy;
    
    // Iterator
    int i = 0;
    
    // Constant register
    const __m256d rfac  = _mm256_set1_pd( 0.7071067811865475 );
    const __m256d rsign = _mm256_set_pd( -0., 0., -0., 0. );
    
    // Registers to be used
    __m256d r00, r01, r02, r03;
    
    // Main loop, no unroll as n is likely low
    for ( ; i <= n-2; i += 2 ) {
        
        r00 = _mm256_loadu_pd( px );
        r01 = _mm256_loadu_pd( py );
        
        r02 = _mm256_sub_pd( r00, r01 );
        r03 = _mm256_add_pd( r00, r01 );
        
        r03 = _mm256_permute_pd( r03, 0x05 );
        r02 = _mm256_mul_pd( r02, rfac );
        
        r03 = _mm256_mul_pd( r03, rfac );
        
        _mm256_storeu_pd( px, r02 );

        r03 = _mm256_xor_pd( r03, rsign );
        
        _mm256_storeu_pd( py, r03 );
        
        px += 4;
        py += 4;
        
    }
    
    // Last SSE step if needed
    if ( i < n ) {
        
        // Constant register
        const __m128d sfac  = _mm256_castpd256_pd128( rfac );
        const __m128d ssign = _mm256_castpd256_pd128( rsign );
        
        // Other registers to be used
        __m128d s00, s01, s02, s03;
        
        s00 = _mm_loadu_pd( px );
        s01 = _mm_loadu_pd( py );
        
        s02 = _mm_sub_pd( s00, s01 );
        s03 = _mm_add_pd( s00, s01 );
        
        s03 = _mm_shuffle_pd( s03, s03, 1 );
        s02 = _mm_mul_pd( s02, sfac );
        
        s03 = _mm_mul_pd( s03, sfac );
        
        _mm_storeu_pd( px, s02 );
        
        s03 = _mm_xor_pd( s03, ssign );
        
        _mm_storeu_pd( py, s03 );
        
    }
    
}
#else
{
    
    // Casting memory addresses
    double *restrict px = ( double * ) cx;
    double *restrict py = ( double * ) cy;
    
    // Iterator
    int i = 0;
    
    // Constant register
    const __m512d rfac  = _mm512_set1_pd( 0.7071067811865475 );
    const __m512d rsign = _mm512_set_pd( -0., 0., -0., 0., -0., 0., -0., 0.);
    
    // Registers to be used
    __m512d r00, r01, r02, r03;
    
    // Main loop, no unroll as n is likely low
    for ( ; i <= n-4; i += 4 ) {
        
        r00 = _mm512_loadu_pd( px );
        r01 = _mm512_loadu_pd( py );
        
        r02 = _mm512_sub_pd( r00, r01 );
        r03 = _mm512_add_pd( r00, r01 );
        
        r03 = _mm512_permute_pd( r03, 0x55 );
        r02 = _mm512_mul_pd( r02, rfac );
        
        r03 = _mm512_mul_pd( r03, rfac );
        
        _mm512_storeu_pd( px, r02 );

        r03 = _mm512_xor_pd( r03, rsign );
        
        _mm512_storeu_pd( py, r03 );
        
        px += 8;
        py += 8;
        
    }
    
    // Masked remainder if needed
    if ( i < n ) {
        
        const __mmask8 mask = _cvtu32_mask8( (1U << ( 2 * ( n - i ) ) ) - 1 );
        
        r00 = _mm512_maskz_loadu_pd( mask, px );
        r01 = _mm512_maskz_loadu_pd( mask, py );
        
        r02 = _mm512_sub_pd( r00, r01 );
        r03 = _mm512_add_pd( r00, r01 );
        
        r03 = _mm512_permute_pd( r03, 0x55 );
        
        r02 = _mm512_mul_pd( r02, rfac );
        r03 = _mm512_mul_pd( r03, rfac );
        
        _mm512_mask_storeu_pd( px, mask, r02 );
        
        r03 = _mm512_xor_pd( r03, rsign );
        
        _mm512_mask_storeu_pd( py, mask, r03 );
        
    }
    
}
#endif