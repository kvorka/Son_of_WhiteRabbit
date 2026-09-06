#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void xy2ee_c( const int n,
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
    const __m256d rsign = _mm256_set_pd( 0., -0., 0., -0. );
    
    // Registers to be used
    __m256d r00, r01, r02, r03, r04, r05, r06, r07;
    
    for ( ; i <= n-4; i += 4 ) {
        
        r02 = _mm256_loadu_pd( py + 0 );
        r03 = _mm256_loadu_pd( py + 4 );
        
        r02 = _mm256_permute_pd( r02, 0x05 );
        r03 = _mm256_permute_pd( r03, 0x05 );
        
        r00 = _mm256_loadu_pd( px + 0 );
        r01 = _mm256_loadu_pd( px + 4 );
        
        r02 = _mm256_xor_pd( r02, rsign );
        r03 = _mm256_xor_pd( r03, rsign );
        r00 = _mm256_mul_pd( r00, rfac  );
        r01 = _mm256_mul_pd( r01, rfac  );
        
        #if defined ( __FMA__ )
        r04 = _mm256_fmadd_pd( rfac, r02, r00 );
        r06 = _mm256_fmadd_pd( rfac, r03, r01 );
        
        _mm256_storeu_pd( px + 0, r04 );
        _mm256_storeu_pd( px + 4, r06 );
        
        r05 = _mm256_fmsub_pd( rfac, r02, r00 );
        r07 = _mm256_fmsub_pd( rfac, r03, r01 );
        
        _mm256_storeu_pd( py + 0, r05 );
        _mm256_storeu_pd( py + 4, r07 );
        #else
        r02 = _mm256_mul_pd( r02, rfac );
        r03 = _mm256_mul_pd( r03, rfac );
        
        r04 = _mm256_add_pd( r02, r00 );
        r06 = _mm256_add_pd( r03, r01 );
        
        _mm256_storeu_pd( px + 0, r04 );
        _mm256_storeu_pd( px + 4, r06 );
        
        r05 = _mm256_sub_pd( r02, r00 );
        r07 = _mm256_sub_pd( r03, r01 );
        
        _mm256_storeu_pd( py + 0, r05 );
        _mm256_storeu_pd( py + 4, r07 );
        #endif
        
        px += 8;
        py += 8;
        
    }
    
    // Remainder (non-loop)
    if ( i <= n-2 ) {
        
        r00 = _mm256_loadu_pd( px );
        r02 = _mm256_loadu_pd( py );
        
        r00 = _mm256_mul_pd( rfac, r00 );
        r02 = _mm256_permute_pd( r02, 0x05 );
        
        r02 = _mm256_xor_pd( r02, rsign );
        
        #if defined ( __FMA__ )
        r04 = _mm256_fmadd_pd( rfac, r02, r00 );
        r05 = _mm256_fmsub_pd( rfac, r02, r00 );
        #else
        r02 = _mm256_mul_pd( rfac, r02 );
        
        r04 = _mm256_add_pd( r02, r00 );
        r05 = _mm256_sub_pd( r02, r00 );
        #endif
        
        _mm256_storeu_pd( px, r04 );
        _mm256_storeu_pd( py, r05 );
        
        px += 4;
        py += 4;
        
        i += 2;
        
    }
    
    // Last SSE step if needed
    if ( i < n ) {
        
        // Constant register
        const __m128d sfac  = _mm256_castpd256_pd128( rfac );
        const __m128d ssign = _mm256_castpd256_pd128( rsign );
        
        // Other registers to be used
        __m128d s00, s02, s04, s05;
        
        s00 = _mm_loadu_pd( px );
        s02 = _mm_loadu_pd( py );
        
        s00 = _mm_mul_pd( sfac, s00 );
        s02 = _mm_shuffle_pd( s02, s02, 1 );
        
        s02 = _mm_xor_pd( s02, ssign );
        
        #if defined ( __FMA__ )
        s04 = _mm_fmadd_pd( sfac, s02, s00 );
        s05 = _mm_fmsub_pd( sfac, s02, s00 );
        #else
        s02 = _mm_mul_pd( sfac, s02 );
        
        s04 = _mm_add_pd( s02, s00 );
        s05 = _mm_sub_pd( s02, s00 );
        #endif
        
        _mm_storeu_pd( px, s04 );
        _mm_storeu_pd( py, s05 );
        
    }
    
}
#else
{
    
    // Casting memory addresses
    double *restrict px = ( double * ) cx;
    double *restrict py = ( double * ) cy;
    
    // Constant register
    const __m512d rfac  = _mm512_set1_pd( 0.7071067811865475 );
    const __m512d rsign = _mm512_set_pd( 0., -0., 0., -0., 0., -0., 0., -0. );
    
    // Registers to be used
    __m512d r00, r01, r02, r03, 
            r04, r05, r06, r07,
            r10, r11, r12, r13, 
            r14, r15, r16, r17;
    
    // Iterator
    int i = 0;
    
    // Main loop
    for ( ; i <= n-16; i += 16 ) {
        
        r02 = _mm512_loadu_pd( py + 0 );
        r03 = _mm512_loadu_pd( py + 8 );
        r12 = _mm512_loadu_pd( py + 16 );
        r13 = _mm512_loadu_pd( py + 24 );
        
        r02 = _mm512_permute_pd( r02, 0x55 );
        r03 = _mm512_permute_pd( r03, 0x55 );
        r12 = _mm512_permute_pd( r12, 0x55 );
        r13 = _mm512_permute_pd( r13, 0x55 );
        
        r00 = _mm512_loadu_pd( px + 0 );
        r01 = _mm512_loadu_pd( px + 8 );
        r10 = _mm512_loadu_pd( px + 16 );
        r11 = _mm512_loadu_pd( px + 24 );
        
        r02 = _mm512_xor_pd( r02, rsign );
        r03 = _mm512_xor_pd( r03, rsign );
        r12 = _mm512_xor_pd( r12, rsign );
        r13 = _mm512_xor_pd( r13, rsign );
        
        r00 = _mm512_mul_pd( r00, rfac  );
        r01 = _mm512_mul_pd( r01, rfac  );
        r10 = _mm512_mul_pd( r10, rfac  );
        r11 = _mm512_mul_pd( r11, rfac  );
        
        r04 = _mm512_fmadd_pd( rfac, r02, r00 );
        r06 = _mm512_fmadd_pd( rfac, r03, r01 );
        r14 = _mm512_fmadd_pd( rfac, r12, r10 );
        r16 = _mm512_fmadd_pd( rfac, r13, r11 );
        
        _mm512_storeu_pd( px +  0, r04 );
        _mm512_storeu_pd( px +  8, r06 );
        _mm512_storeu_pd( px + 16, r14 );
        _mm512_storeu_pd( px + 24, r16 );
        
        r05 = _mm512_fmsub_pd( rfac, r02, r00 );
        r07 = _mm512_fmsub_pd( rfac, r03, r01 );
        r15 = _mm512_fmsub_pd( rfac, r12, r10 );
        r17 = _mm512_fmsub_pd( rfac, r13, r11 );
        
        _mm512_storeu_pd( py +  0, r05 );
        _mm512_storeu_pd( py +  8, r07 );
        _mm512_storeu_pd( py + 16, r15 );
        _mm512_storeu_pd( py + 24, r17 );
        
        px += 32;
        py += 32;
        
    }
    
    for ( ; i <= n-4; i += 4 ) {
        
        r00 = _mm512_loadu_pd( px );
        r02 = _mm512_loadu_pd( py );
        
        r02 = _mm512_permute_pd( r02, 0x55 );
        
        r00 = _mm512_mul_pd( rfac, r00 );
        r02 = _mm512_xor_pd( r02, rsign );
        
        r04 = _mm512_fmadd_pd( rfac, r02, r00 );
        r05 = _mm512_fmsub_pd( rfac, r02, r00 );
        
        _mm512_storeu_pd( px, r04 );
        _mm512_storeu_pd( py, r05 );
        
        px += 8;
        py += 8;
        
    }
    
    // Masked remainder
    if ( i < n ) {
        
        const __mmask8 mask = _cvtu32_mask8( (1U << ( 2 * ( n - i ) )) - 1 );
        
        r00 = _mm512_maskz_loadu_pd( mask, px );
        r02 = _mm512_maskz_loadu_pd( mask, py );
        
        r02 = _mm512_permute_pd( r02, 0x55 );
        
        r00 = _mm512_mul_pd( r00, rfac  );
        r02 = _mm512_xor_pd( r02, rsign );
        
        r04 = _mm512_fmadd_pd( rfac, r02, r00 );
        r05 = _mm512_fmsub_pd( rfac, r02, r00 );
        
        _mm512_mask_storeu_pd( px, mask, r04 );
        _mm512_mask_storeu_pd( py, mask, r05 );
        
    }
    
}
#endif