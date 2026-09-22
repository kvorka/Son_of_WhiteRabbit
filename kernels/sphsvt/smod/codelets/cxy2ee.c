#include "../../../math/cvec.h"

extern inline __attribute__((always_inline))
void xy2ee_c( const int n,
                    double complex *restrict cx,
                    double complex *restrict cy )

{
    
    // Casting memory addresses
    double *restrict px = ( double * ) cx;
    double *restrict py = ( double * ) cy;
    
    // Iterator
    int i = 0;
    
    // Constant register
    const __td rfac  = _t_set1_pd( 0.7071067811865475 );
    const __td rsign = _t_setaltz_pd();
    
    // Registers to be used
    __td r00, r01, r02, r03, r04, r05, r06, r07;
    
    for ( ; i <= n-vlen; i += vlen ) {
        
        r02 = _t_loadu_pd( py        );
        r03 = _t_loadu_pd( py + vlen );
        
        r02 = _t_permute_pd( r02, cmask );
        r03 = _t_permute_pd( r03, cmask );
        
        r00 = _t_loadu_pd( px        );
        r01 = _t_loadu_pd( px + vlen );
        
        r02 = _t_xor_pd( r02, rsign );
        r03 = _t_xor_pd( r03, rsign );
        r00 = _t_mul_pd( r00, rfac  );
        r01 = _t_mul_pd( r01, rfac  );
        
        #if defined ( __FMA__ ) || defined (__AVX512F__)
        r04 = _t_fmadd_pd( rfac, r02, r00 );
        r06 = _t_fmadd_pd( rfac, r03, r01 );
        
        _t_storeu_pd( px,        r04 );
        _t_storeu_pd( px + vlen, r06 );
        
        r05 = _t_fmsub_pd( rfac, r02, r00 );
        r07 = _t_fmsub_pd( rfac, r03, r01 );
        
        _t_storeu_pd( py,        r05 );
        _t_storeu_pd( py + vlen, r07 );
        #else
        r02 = _t_mul_pd( r02, rfac );
        r03 = _t_mul_pd( r03, rfac );
        
        r04 = _t_add_pd( r02, r00 );
        r06 = _t_add_pd( r03, r01 );
        
        _t_storeu_pd( px,        r04 );
        _t_storeu_pd( px + vlen, r06 );
        
        r05 = _t_sub_pd( r02, r00 );
        r07 = _t_sub_pd( r03, r01 );
        
        _t_storeu_pd( py,        r05 );
        _t_storeu_pd( py + vlen, r07 );
        #endif
        
        px += vlen2;
        py += vlen2;
        
    }
    
    // Remainder (non-loop)
    if ( i <= n-vlen/2 ) {
        
        r00 = _t_loadu_pd( px );
        r02 = _t_loadu_pd( py );
        
        r00 = _t_mul_pd( rfac, r00 );
        r02 = _t_permute_pd( r02, cmask );
        
        r02 = _t_xor_pd( r02, rsign );
        
        #if defined ( __FMA__ )
        r04 = _t_fmadd_pd( rfac, r02, r00 );
        r05 = _t_fmsub_pd( rfac, r02, r00 );
        #else
        r02 = _t_mul_pd( rfac, r02 );
        
        r04 = _t_add_pd( r02, r00 );
        r05 = _t_sub_pd( r02, r00 );
        #endif
        
        _t_storeu_pd( px, r04 );
        _t_storeu_pd( py, r05 );
        
        px += vlen;
        py += vlen;
        i  += vlen/2;
        
    }
    
    // SSE/AVX512 masked remainders if needed
    if ( i < n ) {
        
        #if !defined (__AVX512F__)
            
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
        
        #else
            
            // Mask for remainder
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
        
        #endif
        
    }
    
}