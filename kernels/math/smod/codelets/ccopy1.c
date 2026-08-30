#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void copy1_carray_c( const int length,
                     const double *restrict fac,
                           double complex *restrict arr )

#if defined ( mem32 )
{
    
    // Casting memory addresses
    double *pa = ( double * ) arr;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant register
        const __m256d rfac = _mm256_broadcast_sd( fac );
        
        // Registers to be used
        __m256d r0, r1, r2, r3;
        
        for ( ; i <= length-8; i += 8 ) {
            
            r0 = _mm256_loadu_pd( pa +  0 );
            r1 = _mm256_loadu_pd( pa +  4 );
            r2 = _mm256_loadu_pd( pa +  8 );
            r3 = _mm256_loadu_pd( pa + 12 );
            
            r0 = _mm256_mul_pd( rfac, r0 );
            r1 = _mm256_mul_pd( rfac, r1 );
            r2 = _mm256_mul_pd( rfac, r2 );
            r3 = _mm256_mul_pd( rfac, r3 );
            
            _mm256_storeu_pd( pa +  0, r0 );
            _mm256_storeu_pd( pa +  4, r1 );
            _mm256_storeu_pd( pa +  8, r2 );
            _mm256_storeu_pd( pa + 12, r3 );
            
            pa += 16;
            
        }
        
        // Remainder loop
        for ( ; i <= length-2; i += 2 ) {
            
            _mm256_storeu_pd( pa, _mm256_mul_pd( rfac, _mm256_loadu_pd( pa ) ) );
            
            pa += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i < length ) {
        
        _mm_storeu_pd( pa, _mm_mul_pd( _mm_load1_pd( fac ), _mm_loadu_pd( pa ) ) );
        
    }
    
}
#else
{
    
    // Casting memory addresses
    double *pa = ( double * ) arr;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant register
        const __m512d rfac = _mm512_set1_pd( *fac );
        
        // Registers to be used
        __m512d r0, r1, r2, r3;
        
        for ( ; i <= length-16; i += 16 ) {
            
            r0 = _mm512_loadu_pd( pa +  0 );
            r1 = _mm512_loadu_pd( pa +  8 );
            r2 = _mm512_loadu_pd( pa + 16 );
            r3 = _mm512_loadu_pd( pa + 24 );
            
            r0 = _mm512_mul_pd( rfac, r0 );
            r1 = _mm512_mul_pd( rfac, r1 );
            r2 = _mm512_mul_pd( rfac, r2 );
            r3 = _mm512_mul_pd( rfac, r3 );
            
            _mm512_storeu_pd( pa +  0, r0 );
            _mm512_storeu_pd( pa +  8, r1 );
            _mm512_storeu_pd( pa + 16, r2 );
            _mm512_storeu_pd( pa + 24, r3 );
            
            pa += 32;
            
        }
        
        // Remainder loop
        for ( ; i <= length-4; i += 4 ) {
            
            _mm512_storeu_pd( pa, _mm512_mul_pd( rfac, _mm512_loadu_pd( pa ) ) );
            
            pa += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < length ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
        
        for ( ; i < length; i++ ) {
            
            _mm_storeu_pd( pa, _mm_mul_pd( rfac, _mm_loadu_pd( pa ) ) );
            
            pa += 2;
            
        }
        
    }
    
}
#endif