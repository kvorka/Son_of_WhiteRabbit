#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void zero_carray_c( const int length,
                          double complex *restrict arr )

#if defined ( mem32 )
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    double *pa = ( double * ) arr;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant zero register
        const __m256d rzero = _mm256_setzero_pd();
        
        for ( ; i <= n2-16; i += 16 ) {
            
            _mm256_storeu_pd( pa +  0, rzero );
            _mm256_storeu_pd( pa +  4, rzero );
            _mm256_storeu_pd( pa +  8, rzero );
            _mm256_storeu_pd( pa + 12, rzero );
            
            pa += 16;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-4; i += 4 ) {
            
            _mm256_storeu_pd( pa, rzero );
            
            pa += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i <= n2-2 ) {
        
        _mm_storeu_pd( pa, _mm_setzero_pd() );
        
    }
    
}
#else
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    double *pa = ( double * ) arr;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant zero register
        const __m512d rzero = _mm512_setzero_pd();
        
        for ( ; i <= n2-32; i += 32 ) {
            
            _mm512_storeu_pd( pa +  0, rzero );
            _mm512_storeu_pd( pa +  8, rzero );
            _mm512_storeu_pd( pa + 16, rzero );
            _mm512_storeu_pd( pa + 24, rzero );
            
            pa += 32;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-8; i += 8 ) {
            
            _mm512_storeu_pd( pa, rzero );
            
            pa += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        // Constant zero register
        const __m128d rzero = _mm_setzero_pd();
        
        for ( ; i <= n2-2; i += 2 ) {
            
            _mm_storeu_pd( pa, rzero );
            
            pa += 2;
            
        }
        
    }
    
}
#endif