#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void copy2_carray_c( const int length,
                     const double *restrict fac,
                     const double complex *restrict arr_from,
                           double complex *restrict arr_to )

#if defined ( mem32 )
{
    
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
        __m256d r0, r1, r2, r3;
        
        for ( ; i <= length-8; i += 8 ) {
            
            r0 = _mm256_loadu_pd( pf +  0 );
            r1 = _mm256_loadu_pd( pf +  4 );
            r2 = _mm256_loadu_pd( pf +  8 );
            r3 = _mm256_loadu_pd( pf + 12 );
            
            r0 = _mm256_mul_pd( rfac, r0 );
            r1 = _mm256_mul_pd( rfac, r1 );
            r2 = _mm256_mul_pd( rfac, r2 );
            r3 = _mm256_mul_pd( rfac, r3 );
            
            _mm256_storeu_pd( pt +  0, r0 );
            _mm256_storeu_pd( pt +  4, r1 );
            _mm256_storeu_pd( pt +  8, r2 );
            _mm256_storeu_pd( pt + 12, r3 );
            
            pf += 16;
            pt += 16;
            
        }
        
        // Remainder loop
        for ( ; i <= length-2; i += 2 ) {
            
            _mm256_storeu_pd( pt, _mm256_mul_pd( rfac, _mm256_loadu_pd( pf ) ) );
            
            pf += 4;
            pt += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i < length ) {
        
        _mm_storeu_pd( pt, _mm_mul_pd( _mm_load1_pd( fac ), _mm_loadu_pd( pf ) ) );
        
    }
    
}
#else
{
    
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
        __m512d r0, r1, r2, r3;
        
        for ( ; i <= length-16; i += 16 ) {
            
            r0 = _mm512_loadu_pd( pf +  0 );
            r1 = _mm512_loadu_pd( pf +  8 );
            r2 = _mm512_loadu_pd( pf + 16 );
            r3 = _mm512_loadu_pd( pf + 24 );
            
            r0 = _mm512_mul_pd( rfac, r0 );
            r1 = _mm512_mul_pd( rfac, r1 );
            r2 = _mm512_mul_pd( rfac, r2 );
            r3 = _mm512_mul_pd( rfac, r3 );
            
            _mm512_storeu_pd( pt +  0, r0 );
            _mm512_storeu_pd( pt +  8, r1 );
            _mm512_storeu_pd( pt + 16, r2 );
            _mm512_storeu_pd( pt + 24, r3 );
            
            pf += 32;
            pt += 32;
            
        }
        
        // Remainder loop
        for ( ; i <= length-4; i += 4 ) {
            
            _mm512_storeu_pd( pt, _mm512_mul_pd( rfac, _mm512_loadu_pd( pf ) ) );
            
            pf += 8;
            pt += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < length ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
        
        for ( ; i < length; i++ ) {
            
            _mm_storeu_pd( pt, _mm_mul_pd( rfac, _mm_loadu_pd( pf ) ) );
            
            pf += 2;
            pt += 2;
            
        }
        
    }
    
}
#endif