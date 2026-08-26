#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void copy_carray_c( const int length,
                    const double complex *restrict arr_from,
                          double complex *restrict arr_to )

#if defined ( mem32 )
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    const double *pfrom = ( const double * ) arr_from;
          double *pto   = (       double * ) arr_to;
    
    // Memory addresses to be used
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
        
        // Registers to be used
        __m256d r0, r1, r2, r3;
        
        // Loop unrolled by 4
        for ( ; i <= n2-16; i += 16 ) {
            
            r0 = _mm256_loadu_pd( pf0 );
            r1 = _mm256_loadu_pd( pf1 );
            r2 = _mm256_loadu_pd( pf2 );
            r3 = _mm256_loadu_pd( pf3 );
            
            _mm256_storeu_pd( pt0, r0 );
            _mm256_storeu_pd( pt1, r1 );
            _mm256_storeu_pd( pt2, r2 );
            _mm256_storeu_pd( pt3, r3 );
            
            pf0 += 16;
            pf1 += 16;
            pf2 += 16;
            pf3 += 16;
            
            pt0 += 16;
            pt1 += 16;
            pt2 += 16;
            pt3 += 16;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-4; i += 4 ) {
            
            _mm256_storeu_pd( pt0, _mm256_loadu_pd( pf0 ) );
            
            pf0 += 4;
            pt0 += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i <= n2-2 ) {
        
        _mm_storeu_pd( pt0, _mm_loadu_pd( pf0 ) );
        
    }
    
}
#else
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    const double *pfrom = ( const double * ) arr_from;
          double *pto   = (       double * ) arr_to;
    
    // Memory addresses to be used
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
        
        // Registers to be used
        __m512d r0, r1, r2, r3;
        
        // Loop unrolled by 4
        for ( ; i <= n2-32; i += 32 ) {
            
            r0 = _mm512_loadu_pd( pf0 );
            r1 = _mm512_loadu_pd( pf1 );
            r2 = _mm512_loadu_pd( pf2 );
            r3 = _mm512_loadu_pd( pf3 );
            
            _mm512_storeu_pd( pt0, r0 );
            _mm512_storeu_pd( pt1, r1 );
            _mm512_storeu_pd( pt2, r2 );
            _mm512_storeu_pd( pt3, r3 );
            
            pf0 += 32;
            pf1 += 32;
            pf2 += 32;
            pf3 += 32;
            
            pt0 += 32;
            pt1 += 32;
            pt2 += 32;
            pt3 += 32;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-8; i += 8 ) {
            
            _mm512_storeu_pd( pt0, _mm512_loadu_pd( pf0 ) );
            
            pf0 += 8;
            pt0 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        for ( ; i <= n2-2; i += 2 ) {
            
            _mm_storeu_pd( pt0, _mm_loadu_pd( pf0 ) );
            
            pt0 += 2;
            pf0 += 2;
            
        }
        
    }
    
}
#endif