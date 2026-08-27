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
    const double *pf = ( const double * ) arr_from;
          double *pt = (       double * ) arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m256d r0, r1, r2, r3;
        
        // Loop unrolled by 4
        for ( ; i <= n2-16; i += 16 ) {
            
            r0 = _mm256_loadu_pd( pf +  0 );
            r1 = _mm256_loadu_pd( pf +  4 );
            r2 = _mm256_loadu_pd( pf +  8 );
            r3 = _mm256_loadu_pd( pf + 12 );
            
            _mm256_storeu_pd( pt +  0, r0 );
            _mm256_storeu_pd( pt +  4, r1 );
            _mm256_storeu_pd( pt +  8, r2 );
            _mm256_storeu_pd( pt + 12, r3 );
            
            pf += 16;
            pt += 16;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-4; i += 4 ) {
            
            _mm256_storeu_pd( pt, _mm256_loadu_pd( pf ) );
            
            pf += 4;
            pt += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i <= n2-2 ) {
        
        _mm_storeu_pd( pt, _mm_loadu_pd( pf ) );
        
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
        
        // Registers to be used
        __m512d r0, r1, r2, r3;
        
        // Loop unrolled by 4
        for ( ; i <= n2-32; i += 32 ) {
            
            r0 = _mm512_loadu_pd( pf +  0 );
            r1 = _mm512_loadu_pd( pf +  8 );
            r2 = _mm512_loadu_pd( pf + 16 );
            r3 = _mm512_loadu_pd( pf + 24 );
            
            _mm512_storeu_pd( pt +  0, r0 );
            _mm512_storeu_pd( pt +  8, r1 );
            _mm512_storeu_pd( pt + 16, r2 );
            _mm512_storeu_pd( pt + 24, r3 );
            
            pf += 32;
            pt += 32;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-8; i += 8 ) {
            
            _mm512_storeu_pd( pt, _mm512_loadu_pd( pf ) );
            
            pf += 8;
            pt += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        for ( ; i <= n2-2; i += 2 ) {
            
            _mm_storeu_pd( pt, _mm_loadu_pd( pf ) );
            
            pt += 2;
            pf += 2;
            
        }
        
    }
    
}
#endif