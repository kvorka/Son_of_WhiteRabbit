#include "../../cvec.h"

extern inline __attribute__((always_inline))
void ee2xy_c( const int n,
                    double complex *restrict cx,
                    double complex *restrict cy )

{
    
    // Casting memory addresses
    double *restrict px = ( double * ) cx;
    double *restrict py = ( double * ) cy;
    
    // Constant register
    const __td rfac  = _t_set1_pd( 0.7071067811865475 );
    const __td rsign = _t_setalts_pd();
    
    // Registers to be used
    __td r00, r01, r02, r03,
         r10, r11, r12, r13;
    
    // Iterator
    int i = 0;
    
    // Main loop, unrolled only by 4 cmplx numbers for avx
    // and 8 cmplx numbers for avx512 as n is likely low
    for ( ; i <= n-vlen; i += vlen ) {
        
        r00 = _t_loadu_pd( px        );
        r10 = _t_loadu_pd( px + vlen );
        r01 = _t_loadu_pd( py        );
        r11 = _t_loadu_pd( py + vlen );
        
        r02 = _t_sub_pd( r00, r01 );
        r03 = _t_add_pd( r00, r01 );
        r12 = _t_sub_pd( r10, r11 );
        r13 = _t_add_pd( r10, r11 );
        
        r03 = _t_permute_pd( r03, cmask );
        r13 = _t_permute_pd( r13, cmask );
        r02 = _t_mul_pd( r02, rfac );
        r12 = _t_mul_pd( r12, rfac );
        
        r03 = _t_mul_pd( r03, rfac );
        r13 = _t_mul_pd( r13, rfac );
        
        _t_storeu_pd( px       , r02 );
        _t_storeu_pd( px + vlen, r12 );
        
        r03 = _t_xor_pd( r03, rsign );
        r13 = _t_xor_pd( r13, rsign );
        
        _t_storeu_pd( py       , r03 );
        _t_storeu_pd( py + vlen, r13 );
        
        px += 2*vlen;
        py += 2*vlen;
        
    }
    
    // Remainder (non-loop)
    if ( i < n-vlen/2 ) {
        
        r00 = _t_loadu_pd( px );
        r01 = _t_loadu_pd( py );
        
        r02 = _t_sub_pd( r00, r01 );
        r03 = _t_add_pd( r00, r01 );
        
        r03 = _t_permute_pd( r03, cmask );
        r02 = _t_mul_pd( r02, rfac );
        
        r03 = _t_mul_pd( r03, rfac );
        
        _t_storeu_pd( px, r02 );
        
        r03 = _t_xor_pd( r03, rsign );
        
        _t_storeu_pd( py, r03 );
        
        px += vlen;
        py += vlen;
        i  += vlen/2;
        
    }
    
    // SSE/AVX512 masked remainders if needed
    if ( i < n ) {
        
        #if !defined (__AVX512F__)
            
            // Constant register mapping
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
            
        #else
            
            // Mask for remainder
            const __mmask8 mask = _cvtu32_mask8( ( 1U << ( 2 * ( n - i ) ) ) - 1 );
            
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
        
        #endif
        
    }
    
}