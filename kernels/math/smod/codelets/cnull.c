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
    double *parr = ( double * ) arr;
    
    // Other memory addresses
    double *p0 = parr +  0;
    double *p1 = parr +  4;
    double *p2 = parr +  8;
    double *p3 = parr + 12;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant zero register
        const __m256d rzero = _mm256_setzero_pd();
        
        for ( ; i <= n2-16; i += 16 ) {
            
            _mm256_storeu_pd( p0, rzero );
            _mm256_storeu_pd( p1, rzero );
            _mm256_storeu_pd( p2, rzero );
            _mm256_storeu_pd( p3, rzero );
            
            p0 += 16;
            p1 += 16;
            p2 += 16;
            p3 += 16;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-4; i += 4 ) {
            
            _mm256_storeu_pd( p0, rzero );
            
            p0 += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i <= n2-2 ) {
        
        _mm_storeu_pd( p0, _mm_setzero_pd() );
        
    }
    
}
#else
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    double *parr = ( double * ) arr;
    
    // Other memory addresses
    double *p0 = parr +  0;
    double *p1 = parr +  8;
    double *p2 = parr + 16;
    double *p3 = parr + 24;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant zero register
        const __m512d rzero = _mm512_setzero_pd();
        
        for ( ; i <= n2-32; i += 32 ) {
            
            _mm512_storeu_pd( p0, rzero );
            _mm512_storeu_pd( p1, rzero );
            _mm512_storeu_pd( p2, rzero );
            _mm512_storeu_pd( p3, rzero );
            
            p0 += 32;
            p1 += 32;
            p2 += 32;
            p3 += 32;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-8; i += 8 ) {
            
            _mm512_storeu_pd( p0, rzero );
            
            p0 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        // Constant zero register
        const __m128d rzero = _mm_setzero_pd();
        
        for ( ; i <= n2-2; i += 2 ) {
            
            _mm_storeu_pd( p0, rzero );
            
            p0 += 2;
            
        }
        
    }
    
}
#endif