#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void copy_rarray_c( const int istart,
                    const int length,
                    const double *restrict arr_from,
                          double *restrict arr_to )

#if defined ( mem32 )
{
    
    // Starting memory addresses
    const double *pf = arr_from + istart -1;
          double *pt = arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m256d r00, r01, r02, r03;
        
        // Main loop unrolled by 4
        for ( ; i <= length-16; i += 16 ) {
            
            r00 = _mm256_loadu_pd( pf +  0 );
            r01 = _mm256_loadu_pd( pf +  4 );
            r02 = _mm256_loadu_pd( pf +  8 );
            r03 = _mm256_loadu_pd( pf + 12 );
            
            _mm256_storeu_pd( pt +  0, r00 );
            _mm256_storeu_pd( pt +  4, r01 );
            _mm256_storeu_pd( pt +  8, r02 );
            _mm256_storeu_pd( pt + 12, r03 );
            
            pf += 16;
            pt += 16;
            
        }
        
        // Remainer loop
        for ( ; i <= length-4; i += 4 ) {
            
            r00 = _mm256_loadu_pd( pf );
            
            _mm256_storeu_pd( pt, r00 );
            
            pf += 4;
            pt += 4;
            
        }
        
    }
    
    // Last SSE step if possible
    if ( i <= length-2 ) {
        
        _mm_storeu_pd( pt, _mm_loadu_pd( pf ) );
        
        pf += 2;
        pt += 2;
        
        i += 2;
        
    }
    
    // Scalar remainder if needed
    if ( i < length ) { *pt = *pf; }
    
}
#else
{
    
    // Starting memory addresses
    const double *pf = arr_from + istart - 1;
          double *pt = arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m512d r00, r01, r02, r03;
        
        for ( ; i <= length-32; i += 32 ) {
            
            r00 = _mm512_loadu_pd( pf +  0 );
            r01 = _mm512_loadu_pd( pf +  8 );
            r02 = _mm512_loadu_pd( pf + 16 );
            r03 = _mm512_loadu_pd( pf + 24 );
            
            _mm512_storeu_pd( pt +  0, r00 );
            _mm512_storeu_pd( pt +  8, r01 );
            _mm512_storeu_pd( pt + 16, r02 );
            _mm512_storeu_pd( pt + 24, r03 );
            
            pf += 32;
            pt += 32;
            
        }
        
        // Remainer loop
        for ( ; i <= length-8; i += 8 ) {
            
            _mm512_storeu_pd( pt, _mm512_loadu_pd( pf ) );
            
            pt += 8;
            pf += 8;
            
        }
        
        // Masked remainder
        if ( i < length ) {
            
            int rem = length - i;
            
            __mmask8 mask = (1U << rem) - 1U;
            
            _mm512_mask_storeu_pd( pt, mask, _mm512_maskz_loadu_pd( mask, pf ) );
            
        }
        
    }
    
}
#endif