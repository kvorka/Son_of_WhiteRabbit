#include <stddef.h>
#include <complex.h>
#include <emmintrin.h>
#include <immintrin.h>

extern inline __attribute__((always_inline))
void cadj3_carray_c( const int length,
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
        const __m256d radj = _mm256_set_pd( -0., 0., -0., 0. );
        
        // Registers to be used
        __m256d r00, r01, r02, r03,
                r04, r05, r06, r07;
        
        // Main cycle unrolled by 4 with fma instructions
        // if availaible
        for ( ; i <= n2-16; i += 16 ) {
            
            r00 = _mm256_loadu_pd( pf0 );
            r01 = _mm256_loadu_pd( pf1 );
            r02 = _mm256_loadu_pd( pf2 );
            r03 = _mm256_loadu_pd( pf3 );
            
            r00 = _mm256_xor_pd( radj, r00 );
            r01 = _mm256_xor_pd( radj, r01 );
            r02 = _mm256_xor_pd( radj, r02 );
            r03 = _mm256_xor_pd( radj, r03 );
            
            r04 = _mm256_loadu_pd( pt0 );
            r05 = _mm256_loadu_pd( pt1 );
            r06 = _mm256_loadu_pd( pt2 );
            r07 = _mm256_loadu_pd( pt3 );
            
            #if defined (fma)
            r00 = _mm256_fmadd_pd( rfac, r00, r04 );
            r01 = _mm256_fmadd_pd( rfac, r01, r05 );
            r02 = _mm256_fmadd_pd( rfac, r02, r06 );
            r03 = _mm256_fmadd_pd( rfac, r03, r07 );
            #else
            r00 = _mm256_mul_pd( rfac, r00 );
            r01 = _mm256_mul_pd( rfac, r01 );
            r02 = _mm256_mul_pd( rfac, r02 );
            r03 = _mm256_mul_pd( rfac, r03 );
            
            r00 = _mm256_add_pd( r04, r00 );
            r01 = _mm256_add_pd( r05, r01 );
            r02 = _mm256_add_pd( r06, r02 );
            r03 = _mm256_add_pd( r07, r03 );
            #endif
            
            _mm256_storeu_pd( pt0, r00 );
            _mm256_storeu_pd( pt1, r01 );
            _mm256_storeu_pd( pt2, r02 );
            _mm256_storeu_pd( pt3, r03 );
            
            pf0 += 16;
            pf1 += 16;
            pf2 += 16;
            pf3 += 16;
            
            pt0 += 16;
            pt1 += 16;
            pt2 += 16;
            pt3 += 16;
            
        }
        
        // Remainder loop without fma as there is no chance
        // of hiding the latency
        for ( ; i <= n2-4; i += 4 ) {
            
            r00 = _mm256_loadu_pd( pf0 );
            r04 = _mm256_loadu_pd( pt0 );
            
            r00 = _mm256_xor_pd( radj, r00 );
            r00 = _mm256_mul_pd( rfac, r00 );
            r00 = _mm256_add_pd( r00, r04 );
            
            _mm256_storeu_pd( pt0, r00 );
            
            pf0 += 4;
            pt0 += 4;
            
        }
        
    }
    
    // Last SSE step if needed, againg, without fma
    if ( i <= n2-2 ) {
        
        const __m128d rs00 = _mm_set_pd( -0., 0.);
        const __m128d rs01 = _mm_load1_pd( fac );
        
        __m128d rs02 = _mm_loadu_pd( pf0 );
        __m128d rs03 = _mm_loadu_pd( pt0 );
        
        rs02 = _mm_xor_pd( rs00, rs02 );
        rs02 = _mm_mul_pd( rs01, rs02 );
        rs02 = _mm_add_pd( rs02, rs03 );
        
        _mm_storeu_pd( pt0, rs02 );
        
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
        const __m512d rfac = _mm512_set1_pd( *fac );
        const __m512d radj = _mm512_set_pd( -0., 0., -0., 0., -0., 0., -0., 0. );
        
        // Registers to be used
        __m512d r00, r01, r02, r03,
                r04, r05, r06, r07;
        
        // Main cycle unrolled by 4 with fma instructions
        // if availaible
        for ( ; i <= n2-32; i += 32 ) {
            
            r00 = _mm512_loadu_pd( pf0 );
            r01 = _mm512_loadu_pd( pf1 );
            r02 = _mm512_loadu_pd( pf2 );
            r03 = _mm512_loadu_pd( pf3 );
            
            r00 = _mm512_xor_pd( radj, r00 );
            r01 = _mm512_xor_pd( radj, r01 );
            r02 = _mm512_xor_pd( radj, r02 );
            r03 = _mm512_xor_pd( radj, r03 );
            
            r04 = _mm512_loadu_pd( pt0 );
            r05 = _mm512_loadu_pd( pt1 );
            r06 = _mm512_loadu_pd( pt2 );
            r07 = _mm512_loadu_pd( pt3 );
            
            r00 = _mm512_fmadd_pd( rfac, r00, r04 );
            r01 = _mm512_fmadd_pd( rfac, r01, r05 );
            r02 = _mm512_fmadd_pd( rfac, r02, r06 );
            r03 = _mm512_fmadd_pd( rfac, r03, r07 );
            
            _mm512_storeu_pd( pt0, r00 );
            _mm512_storeu_pd( pt1, r01 );
            _mm512_storeu_pd( pt2, r02 );
            _mm512_storeu_pd( pt3, r03 );
            
            pf0 += 32;
            pf1 += 32;
            pf2 += 32;
            pf3 += 32;
            
            pt0 += 32;
            pt1 += 32;
            pt2 += 32;
            pt3 += 32;
            
        }
        
        // Remainder loop without fma as there is no chance
        // of hiding the latency
        for ( ; i <= n2-8; i += 8 ) {
            
            r00 = _mm512_loadu_pd( pf0 );
            r04 = _mm512_loadu_pd( pt0 );
            
            r00 = _mm512_xor_pd( radj, r00 );
            r00 = _mm512_mul_pd( rfac, r00 );
            r00 = _mm512_add_pd( r00, r04 );
            
            _mm512_storeu_pd( pt0, r00 );
            
            pf0 += 8;
            pt0 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
        const __m128d radj = _mm_set_pd( -0., 0.);
        
        __m128d r00, r04;
        
        for ( ; i <= n2-2; i += 2 ) {
            
            r00 = _mm_loadu_pd( pf0 );
            r04 = _mm_loadu_pd( pt0 );
            
            r00 = _mm_xor_pd( radj, r00 );
            r00 = _mm_mul_pd( rfac, r00 );
            r00 = _mm_add_pd( r00, r04 );
            
            _mm_storeu_pd( pt0, r00 );
            
            pf0 += 2;
            pt0 += 2;
            
        }
        
    }
    
}
#endif