#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void copy3_carray_c( const int length,
                     const double *restrict fac,
                     const double complex *restrict arr_from,
                           double complex *restrict arr_to )

#if defined ( mem32 )
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    const double *pf = ( const double * ) arr_from;
          double *pt = (       double * ) arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m256d rfac = _mm256_broadcast_sd( fac );
        
        // Registers to be used
        __m256d rf0, rf1, rf2, rf3,
                rt0, rt1, rt2, rt3;
        
        // Main loop with fma instructions unrolled by 4
        for ( ; i <= n2-16; i += 16 ) {
            
            rf0 = _mm256_loadu_pd( pf +  0 );
            rf1 = _mm256_loadu_pd( pf +  4 );
            rf2 = _mm256_loadu_pd( pf +  8 );
            rf3 = _mm256_loadu_pd( pf + 12 );
            
            rt0 = _mm256_loadu_pd( pt +  0 );
            rt1 = _mm256_loadu_pd( pt +  4 );
            rt2 = _mm256_loadu_pd( pt +  8 );
            rt3 = _mm256_loadu_pd( pt + 12 );
            
            #if defined (fma)
            rt0 = _mm256_fmadd_pd( rfac, rf0, rt0 );
            rt1 = _mm256_fmadd_pd( rfac, rf1, rt1 );
            rt2 = _mm256_fmadd_pd( rfac, rf2, rt2 );
            rt3 = _mm256_fmadd_pd( rfac, rf3, rt3 );
            #else
            rf0 = _mm256_mul_pd( rfac, rf0 );
            rf1 = _mm256_mul_pd( rfac, rf1 );
            rf2 = _mm256_mul_pd( rfac, rf2 );
            rf3 = _mm256_mul_pd( rfac, rf3 );
            
            rt0 = _mm256_add_pd( rf0, rt0 );
            rt1 = _mm256_add_pd( rf1, rt1 );
            rt2 = _mm256_add_pd( rf2, rt2 );
            rt3 = _mm256_add_pd( rf3, rt3 );
            #endif
            
            _mm256_storeu_pd( pt +  0, rt0 );
            _mm256_storeu_pd( pt +  4, rt1 );
            _mm256_storeu_pd( pt +  8, rt2 );
            _mm256_storeu_pd( pt + 12, rt3 );
            
            pf += 16;
            pt += 16;
            
        }
        
        // Remainder loop without fma instructions
        // as there is nowhere to hide their latency
        for ( ; i <= n2-4; i += 4 ) {
            
            rf0 = _mm256_loadu_pd( pf );
            rt0 = _mm256_loadu_pd( pt );
            
            rt0 = _mm256_add_pd( rt0, _mm256_mul_pd( rfac, rf0 ) );
            
            _mm256_storeu_pd( pt, rt0 );
            
            pf += 4;
            pt += 4;
            
        }
        
    }
    
    // Last SSE step if needed, again, without fma
    if ( i <= n2-2 ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
              __m128d rf0  = _mm_loadu_pd( pf  );
              __m128d rt0  = _mm_loadu_pd( pt  );
        
        rt0 = _mm_add_pd( rt0, _mm_mul_pd( rfac, rf0 ) );
        
        _mm_storeu_pd( pt, rt0 );
        
    }
    
}
#else
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    const double *pf = ( const double * ) arr_from;
          double *pt = (       double * ) arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m512d rfac = _mm512_set1_pd( *fac );
        
        // Registers to be used
        __m512d rf0, rf1, rf2, rf3,
                rt0, rt1, rt2, rt3;
        
        // Main loop with fma instructions unrolled by 4
        for ( ; i <= n2-32; i += 32 ) {
            
            rf0 = _mm512_loadu_pd( pf +  0 );
            rf1 = _mm512_loadu_pd( pf +  8 );
            rf2 = _mm512_loadu_pd( pf + 16 );
            rf3 = _mm512_loadu_pd( pf + 24 );
            
            rt0 = _mm512_loadu_pd( pt +  0 );
            rt1 = _mm512_loadu_pd( pt +  8 );
            rt2 = _mm512_loadu_pd( pt + 16 );
            rt3 = _mm512_loadu_pd( pt + 24 );
            
            rt0 = _mm512_fmadd_pd( rfac, rf0, rt0 );
            rt1 = _mm512_fmadd_pd( rfac, rf1, rt1 );
            rt2 = _mm512_fmadd_pd( rfac, rf2, rt2 );
            rt3 = _mm512_fmadd_pd( rfac, rf3, rt3 );
            
            _mm512_storeu_pd( pt +  0, rt0 );
            _mm512_storeu_pd( pt +  8, rt1 );
            _mm512_storeu_pd( pt + 16, rt2 );
            _mm512_storeu_pd( pt + 24, rt3 );
            
            pf += 32;
            pt += 32;
            
        }
        
        // Remainder loop without fma instructions
        // as there is nowhere to hide their latency
        for ( ; i <= n2-8; i += 8 ) {
            
            rf0 = _mm512_loadu_pd( pf );
            rt0 = _mm512_loadu_pd( pt );
            
            rt0 = _mm512_add_pd( rt0, _mm512_mul_pd( rfac, rf0 ) );
            
            _mm512_storeu_pd( pt, rt0 );
            
            pf += 8;
            pt += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
              __m128d rt0, rf0;
        
        for ( ; i <= n2-2; i += 2 ) {
            
            rf0 = _mm_loadu_pd( pf );
            rt0 = _mm_loadu_pd( pt );

            rt0 = _mm_add_pd( rt0, _mm_mul_pd( rfac, rf0 ) );
            
            _mm_storeu_pd( pt, rt0 );
            
            pf += 2;
            pt += 2;
            
        }
        
    }
    
}
#endif