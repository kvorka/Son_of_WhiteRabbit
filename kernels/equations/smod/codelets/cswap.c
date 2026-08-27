#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void swap_carray_c( const int length,
                          double complex *restrict arr1,
                          double complex *restrict arr2 )

#if defined ( mem32 )
{
    
    // Complex is two doubles and unrolled increment
    const int n2 = 2 * length;
    
    // Casting memory addresses
    double *p1 = ( double * ) arr1;
    double *p2 = ( double * ) arr2;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m256d r01, r11, r21, r31, 
                r02, r12, r22, r32;
        
        // Main cycle unrolled by 4
        for ( ; i <= n2-16; i += 16 ) {
            
            r01 = _mm256_loadu_pd( p1 +  0 );
            r11 = _mm256_loadu_pd( p1 +  4 );
            r21 = _mm256_loadu_pd( p1 +  8 );
            r31 = _mm256_loadu_pd( p1 + 12 );
            
            r02 = _mm256_loadu_pd( p2 +  0 );
            r12 = _mm256_loadu_pd( p2 +  4 );
            r22 = _mm256_loadu_pd( p2 +  8 );
            r32 = _mm256_loadu_pd( p2 + 12 );
            
            _mm256_storeu_pd( p1 +  0, r02 );
            _mm256_storeu_pd( p1 +  4, r12 );
            _mm256_storeu_pd( p1 +  8, r22 );
            _mm256_storeu_pd( p1 + 12, r32 );
            
            _mm256_storeu_pd( p2 +  0, r01 );
            _mm256_storeu_pd( p2 +  4, r11 );
            _mm256_storeu_pd( p2 +  8, r21 );
            _mm256_storeu_pd( p2 + 12, r31 );
            
            p1 += 16;
            p2 += 16;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-4; i += 4 ) {
            
            r01 = _mm256_loadu_pd( p1 );
            r02 = _mm256_loadu_pd( p2 );
            
            _mm256_storeu_pd( p1, r02 );
            _mm256_storeu_pd( p2, r01 );
            
            p1 += 4;
            p2 += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i <= n2-2 ) {
        
        __m128d r01 = _mm_loadu_pd( p1 );
        __m128d r02 = _mm_loadu_pd( p2 );
        
        _mm_storeu_pd( p1, r02 );
        _mm_storeu_pd( p2, r01 );
        
    }
    
}
#else
{
    
    // Complex is two doubles and unrolled increment
    const int n2 = 2 * length;
    
    // Casting memory addresses
    double *p1 = ( double * ) arr1;
    double *p2 = ( double * ) arr2;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m512d r01, r11, r21, r31, 
                r02, r12, r22, r32;
        
        // Main cycle unrolled by 4
        for ( ; i <= n2-32; i += 32 ) {
            
            r01 = _mm512_loadu_pd( p1 +  0 );
            r11 = _mm512_loadu_pd( p1 +  8 );
            r21 = _mm512_loadu_pd( p1 + 16 );
            r31 = _mm512_loadu_pd( p1 + 24 );
            
            r02 = _mm512_loadu_pd( p2 +  0 );
            r12 = _mm512_loadu_pd( p2 +  8 );
            r22 = _mm512_loadu_pd( p2 + 16 );
            r32 = _mm512_loadu_pd( p2 + 24 );
            
            _mm512_storeu_pd( p1 +  0, r02 );
            _mm512_storeu_pd( p1 +  8, r12 );
            _mm512_storeu_pd( p1 + 16, r22 );
            _mm512_storeu_pd( p1 + 24, r32 );
            
            _mm512_storeu_pd( p2 +  0, r01 );
            _mm512_storeu_pd( p2 +  8, r11 );
            _mm512_storeu_pd( p2 + 16, r21 );
            _mm512_storeu_pd( p2 + 24, r31 );
            
            p1 += 32;
            p2 += 32;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-8; i += 8 ) {
            
            r01 = _mm512_loadu_pd( p1 );
            r02 = _mm512_loadu_pd( p2 );
            
            _mm512_storeu_pd( p1, r02 );
            _mm512_storeu_pd( p2, r01 );
            
            p1 += 8;
            p2 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        __m128d r01, r02;
        
        for ( ; i <= n2-2; i += 2 ) {
            
            r01 = _mm_loadu_pd( p1 );
            r02 = _mm_loadu_pd( p2 );
            
            _mm_storeu_pd( p1, r02 );
            _mm_storeu_pd( p2, r01 );
            
            p1 += 2;
            p2 += 2;
            
        }
        
    }
    
}
#endif