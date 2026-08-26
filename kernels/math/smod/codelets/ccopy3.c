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
    const double *pfrom = ( const double * ) arr_from;
          double *pto   = (       double * ) arr_to;
    
    // Other memory addresses
    const double *pf0 = pfrom +  0;
    const double *pf1 = pfrom +  4;
    const double *pf2 = pfrom +  8;
    const double *pf3 = pfrom + 12;
    
    double *pt0 = pto +  0;
    double *pt1 = pto +  4;
    double *pt2 = pto +  8;
    double *pt3 = pto + 12;
    
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
            
            rf0 = _mm256_loadu_pd( pf0 );
            rf1 = _mm256_loadu_pd( pf1 );
            rf2 = _mm256_loadu_pd( pf2 );
            rf3 = _mm256_loadu_pd( pf3 );
            
            rt0 = _mm256_loadu_pd( pt0 );
            rt1 = _mm256_loadu_pd( pt1 );
            rt2 = _mm256_loadu_pd( pt2 );
            rt3 = _mm256_loadu_pd( pt3 );
            
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
            
            _mm256_storeu_pd( pt0, rt0 );
            _mm256_storeu_pd( pt1, rt1 );
            _mm256_storeu_pd( pt2, rt2 );
            _mm256_storeu_pd( pt3, rt3 );
            
            pf0 += 16;
            pf1 += 16;
            pf2 += 16;
            pf3 += 16;
            
            pt0 += 16;
            pt1 += 16;
            pt2 += 16;
            pt3 += 16;
            
        }
        
        // Remainder loop without fma instructions
        // as there is nowhere to hide their latency
        for ( ; i <= n2-4; i += 4 ) {
            
            rf0 = _mm256_loadu_pd( pf0 );
            rt0 = _mm256_loadu_pd( pt0 );
            
            rt0 = _mm256_add_pd( rt0, _mm256_mul_pd( rfac, rf0 ) );
            
            _mm256_storeu_pd( pt0, rt0 );
            
            pf0 += 4;
            pt0 += 4;
            
        }
        
    }
    
    // Last SSE step if needed, again, without fma
    if ( i <= n2-2 ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
              __m128d rf0  = _mm_loadu_pd( pf0 );
              __m128d rt0  = _mm_loadu_pd( pt0 );
        
        rt0 = _mm_add_pd( rt0, _mm_mul_pd( rfac, rf0 ) );
        
        _mm_storeu_pd( pt0, rt0 );
        
    }
    
}
#else
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    const double *pfrom = ( const double * ) arr_from;
          double *pto   = (       double * ) arr_to;
    
    // Other memory addresses
    const double *pf0 = pfrom +  0;
    const double *pf1 = pfrom +  8;
    const double *pf2 = pfrom + 16;
    const double *pf3 = pfrom + 24;
    
    double *pt0 = pto +  0;
    double *pt1 = pto +  8;
    double *pt2 = pto + 16;
    double *pt3 = pto + 24;
    
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
            
            rf0 = _mm512_loadu_pd( pf0 );
            rf1 = _mm512_loadu_pd( pf1 );
            rf2 = _mm512_loadu_pd( pf2 );
            rf3 = _mm512_loadu_pd( pf3 );
            
            rt0 = _mm512_loadu_pd( pt0 );
            rt1 = _mm512_loadu_pd( pt1 );
            rt2 = _mm512_loadu_pd( pt2 );
            rt3 = _mm512_loadu_pd( pt3 );
            
            rt0 = _mm512_fmadd_pd( rfac, rf0, rt0 );
            rt1 = _mm512_fmadd_pd( rfac, rf1, rt1 );
            rt2 = _mm512_fmadd_pd( rfac, rf2, rt2 );
            rt3 = _mm512_fmadd_pd( rfac, rf3, rt3 );
            
            _mm512_storeu_pd( pt0, rt0 );
            _mm512_storeu_pd( pt1, rt1 );
            _mm512_storeu_pd( pt2, rt2 );
            _mm512_storeu_pd( pt3, rt3 );
            
            pf0 += 32;
            pf1 += 32;
            pf2 += 32;
            pf3 += 32;
            
            pt0 += 32;
            pt1 += 32;
            pt2 += 32;
            pt3 += 32;
            
        }
        
        // Remainder loop without fma instructions
        // as there is nowhere to hide their latency
        for ( ; i <= n2-8; i += 8 ) {
            
            rf0 = _mm512_loadu_pd( pf0 );
            rt0 = _mm512_loadu_pd( pt0 );
            
            rt0 = _mm512_add_pd( rt0, _mm512_mul_pd( rfac, rf0 ) );
            
            _mm512_storeu_pd( pt0, rt0 );
            
            pf0 += 8;
            pt0 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
              __m128d rt0, rf0;
        
        for ( ; i <= n2-2; i += 2 ) {
            
            rf0 = _mm_loadu_pd( pf0 );
            rt0 = _mm_loadu_pd( pt0 );

            rt0 = _mm_add_pd( rt0, _mm_mul_pd( rfac, rf0 ) );
            
            _mm_storeu_pd( pt0, rt0 );
            
            pf0 += 2;
            pt0 += 2;
            
        }
        
    }
    
}
#endif