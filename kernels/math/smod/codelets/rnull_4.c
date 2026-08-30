#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void zero_rarray_4_c( const int istart,
                      const int length,
                            double *restrict arr )

#if defined ( mem32 )
{
    
    // Starting memory address
    double *parr = arr + istart;
    
    // Iterator
    int i = istart;
    
    // Constant zero register
    const __m256d rzero = _mm256_setzero_pd();
    
    for ( ; i <= length-16; i += 16 ) {
        
        _mm256_storeu_pd( parr +  0, rzero );
        _mm256_storeu_pd( parr +  4, rzero );
        _mm256_storeu_pd( parr +  8, rzero );
        _mm256_storeu_pd( parr + 12, rzero );
        
        parr += 16;
        
    }
    
    // Remainer loop
    for ( ; i <= length-4; i += 4 ) {
        
        _mm256_storeu_pd( parr, rzero );
        
        parr += 4;
        
    }
    
}
#else
{
    
    // Starting memory address
    double *parr = arr + istart;
    
    // Iterator
    int i = istart;
    
    // Body of the cycle
    {
        
        // Constant zero register
        const __m512d rzero = _mm512_setzero_pd();
        
        for ( ; i <= length-32; i += 32 ) {
            
            _mm512_storeu_pd( parr +  0, rzero );
            _mm512_storeu_pd( parr +  8, rzero );
            _mm512_storeu_pd( parr + 16, rzero );
            _mm512_storeu_pd( parr + 24, rzero );
            
            parr += 32;
            
        }
        
        // Remainer loop
        for ( ; i <= length-8; i += 8 ) {
            
            _mm512_storeu_pd( parr, rzero );
            
            parr += 8;
            
        }
        
        // AVX remainder
        if ( i < length ) {
            
            _mm256_storeu_pd( parr, _mm256_setzero_pd() );
            
        }
        
    }
    
}
#endif