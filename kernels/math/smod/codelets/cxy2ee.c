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
    double *px = ( double * ) cx;
    double *py = ( double * ) cy;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant register
        const __m256d rsqrt2 = _mm256_set1_pd( 0.7071067811865475 );
        const __m256d rsign  = _mm256_set_pd( 0., -0., 0., -0. );
        
        // Registers to be used
        __m256d r00, r01, r02, r03, 
                r04, r05, r06, r07;
        
        for ( ; i <= n-4; i += 4 ) {
            
            r00 = _mm256_loadu_pd( px + 0 );
            r01 = _mm256_loadu_pd( px + 4 );
            r02 = _mm256_loadu_pd( py + 0 );
            r03 = _mm256_loadu_pd( py + 4 );
            
            r00 = _mm256_mul_pd( rsqrt2, r00 );
            r01 = _mm256_mul_pd( rsqrt2, r01 );
            r02 = _mm256_permute_pd( r02, 0x05 );
            r03 = _mm256_permute_pd( r03, 0x05 );
            
            r02 = _mm256_xor_pd( r02, rsign );
            r03 = _mm256_xor_pd( r03, rsign );
            
            #if defined ( fma )
            r04 = _mm256_fmadd_pd( rsqrt2, r02, r00 );
            r05 = _mm256_fmsub_pd( rsqrt2, r02, r00 );
            r06 = _mm256_fmadd_pd( rsqrt2, r03, r01 );
            r07 = _mm256_fmsub_pd( rsqrt2, r03, r01 );
            #else
            r02 = _mm256_mul_pd( rsqrt2, r02 );
            r03 = _mm256_mul_pd( rsqrt2, r03 );
            
            r04 = _mm256_add_pd( r02, r00 );
            r05 = _mm256_sub_pd( r02, r00 );
            r06 = _mm256_add_pd( r03, r01 );
            r07 = _mm256_sub_pd( r03, r01 );
            #endif
            
            _mm256_storeu_pd( px + 0, r04 );
            _mm256_storeu_pd( px + 4, r06 );
            _mm256_storeu_pd( py + 0, r05 );
            _mm256_storeu_pd( py + 4, r07 );
            
            px += 8;
            py += 8;
            
        }
        
        // Remainder (non-loop)
        if ( i <= n-2 ) {
            
            r00 = _mm256_loadu_pd( px );
            r02 = _mm256_loadu_pd( py );
            
            r00 = _mm256_mul_pd( rsqrt2, r00 );
            r02 = _mm256_permute_pd( r02, 0x05 );
            
            r02 = _mm256_xor_pd( r02, rsign );
            
            #if defined ( fma )
            r04 = _mm256_fmadd_pd( rsqrt2, r02, r00 );
            r05 = _mm256_fmsub_pd( rsqrt2, r02, r00 );
            #else
            r02 = _mm256_mul_pd( rsqrt2, r02 );
            
            r04 = _mm256_add_pd( r02, r00 );
            r05 = _mm256_sub_pd( r02, r00 );
            #endif
            
            _mm256_storeu_pd( px, r04 );
            _mm256_storeu_pd( py, r05 );
            
            px += 4;
            py += 4;
            
            i += 2;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i < n ) {
        
        // Constant register
        const __m128d rsqrt2 = _mm_set1_pd( 0.7071067811865475 );
        const __m128d rsign  = _mm_set_pd( 0., -0. );
        
        // Other registers to be used
        __m128d r00, r02, r04, r05;
        
        r00 = _mm_loadu_pd( px );
        r02 = _mm_loadu_pd( py );
        
        r00 = _mm_mul_pd( rsqrt2, r00 );
        r02 = _mm_shuffle_pd( r02, r02, 1 );
        
        r02 = _mm_xor_pd( r02, rsign );
        
        #if defined ( fma )
        r04 = _mm_fmadd_pd( rsqrt2, r02, r00 );
        r05 = _mm_fmsub_pd( rsqrt2, r02, r00 );
        #else
        r02 = _mm_mul_pd( rsqrt2, r02 );
        
        r04 = _mm_add_pd( r02, r00 );
        r05 = _mm_sub_pd( r02, r00 );
        #endif
        
        _mm_storeu_pd( px, r04 );
        _mm_storeu_pd( py, r05 );
        
    }
    
}
#else
{
    
    // Casting memory addresses
    double *px = ( double * ) cx;
    double *py = ( double * ) cy;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant register
        const __m512d rsqrt2 = _mm512_set1_pd( 0.7071067811865475 );
        const __m512d rsign  = _mm512_set_pd( 0., -0., 0., -0., 0., -0., 0., -0. );
        
        // Registers to be used
        __m512d r00, r01, r02, r03, 
                r04, r05, r06, r07;
        
        for ( ; i <= n-8; i += 8 ) {
            
            r00 = _mm512_loadu_pd( px + 0 );
            r01 = _mm512_loadu_pd( px + 8 );
            r02 = _mm512_loadu_pd( py + 0 );
            r03 = _mm512_loadu_pd( py + 8 );
            
            r00 = _mm512_mul_pd( rsqrt2, r00 );
            r01 = _mm512_mul_pd( rsqrt2, r01 );
            r02 = _mm512_permute_pd( r02, 0x55 );
            r03 = _mm512_permute_pd( r03, 0x55 );
            
            r02 = _mm512_xor_pd( r02, rsign );
            r03 = _mm512_xor_pd( r03, rsign );
            
            r04 = _mm512_fmadd_pd( rsqrt2, r02, r00 );
            r05 = _mm512_fmsub_pd( rsqrt2, r02, r00 );
            r06 = _mm512_fmadd_pd( rsqrt2, r03, r01 );
            r07 = _mm512_fmsub_pd( rsqrt2, r03, r01 );
            
            _mm512_storeu_pd( px + 0, r04 );
            _mm512_storeu_pd( px + 8, r06 );
            _mm512_storeu_pd( py + 0, r05 );
            _mm512_storeu_pd( py + 8, r07 );
            
            px += 16;
            py += 16;
            
        }
        
        // Remainder (non-loop)
        if ( i <= n-4 ) {
            
            r00 = _mm512_loadu_pd( px );
            r02 = _mm512_loadu_pd( py );
            
            r00 = _mm512_mul_pd( rsqrt2, r00 );
            r02 = _mm512_permute_pd( r02, 0x55 );
            
            r02 = _mm512_xor_pd( r02, rsign );
            
            r04 = _mm512_fmadd_pd( rsqrt2, r02, r00 );
            r05 = _mm512_fmsub_pd( rsqrt2, r02, r00 );
            
            _mm512_storeu_pd( px, r04 );
            _mm512_storeu_pd( py, r05 );
            
            px += 8;
            py += 8;
            
            i += 4;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < n ) {
        
        // Constant register
        const __m128d rsqrt2 = _mm_set1_pd( 0.7071067811865475 );
        const __m128d rsign  = _mm_set_pd( 0., -0. );
        
        // Other registers to be used
        __m128d r00, r02, r04, r05;
        
        for ( ; i < n; i++ ) {
            
            r00 = _mm_loadu_pd( px );
            r02 = _mm_loadu_pd( py );
            
            r00 = _mm_mul_pd( rsqrt2, r00 );
            r02 = _mm_shuffle_pd( r02, r02, 1 );
            
            r02 = _mm_xor_pd( r02, rsign );
            
            r04 = _mm_fmadd_pd( rsqrt2, r02, r00 );
            r05 = _mm_fmsub_pd( rsqrt2, r02, r00 );
            
            _mm_storeu_pd( px, r04 );
            _mm_storeu_pd( py, r05 );
            
            px += 2;
            py += 2;
            
        }
        
    }
    
}
#endif