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
    
    // Other memory addresses
    double *p01 = p1 +  0;
    double *p11 = p1 +  4;
    double *p21 = p1 +  8;
    double *p31 = p1 + 12;
    
    double *p02 = p2 +  0;
    double *p12 = p2 +  4;
    double *p22 = p2 +  8;
    double *p32 = p2 + 12;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m256d r01, r11, r21, r31, 
                r02, r12, r22, r32;
        
        // Main cycle unrolled by 4
        for ( ; i <= n2-16; i += 16 ) {
            
            r01 = _mm256_loadu_pd( p01 );
            r11 = _mm256_loadu_pd( p11 );
            r21 = _mm256_loadu_pd( p21 );
            r31 = _mm256_loadu_pd( p31 );
            
            r02 = _mm256_loadu_pd( p02 );
            r12 = _mm256_loadu_pd( p12 );
            r22 = _mm256_loadu_pd( p22 );
            r32 = _mm256_loadu_pd( p32 );
            
            _mm256_storeu_pd( p01, r02 );
            _mm256_storeu_pd( p11, r12 );
            _mm256_storeu_pd( p21, r22 );
            _mm256_storeu_pd( p31, r32 );
            
            _mm256_storeu_pd( p02, r01 );
            _mm256_storeu_pd( p12, r11 );
            _mm256_storeu_pd( p22, r21 );
            _mm256_storeu_pd( p32, r31 );
            
            p01 += 16;
            p11 += 16;
            p21 += 16;
            p31 += 16;
            
            p02 += 16;
            p12 += 16;
            p22 += 16;
            p32 += 16;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-4; i += 4 ) {
            
            r01 = _mm256_loadu_pd( p01 );
            r02 = _mm256_loadu_pd( p02 );
            
            _mm256_storeu_pd( p01, r02 );
            _mm256_storeu_pd( p02, r01 );
            
            p01 += 4;
            p02 += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i <= n2-2 ) {
        
        __m128d r01 = _mm_loadu_pd( p01 );
        __m128d r02 = _mm_loadu_pd( p02 );
        
        _mm_storeu_pd( p01, r02 );
        _mm_storeu_pd( p02, r01 );
        
    }
    
}
#else
{
    
    // Complex is two doubles and unrolled increment
    const int n2 = 2 * length;
    
    // Casting memory addresses
    double *p1 = ( double * ) arr1;
    double *p2 = ( double * ) arr2;
    
    // Other memory addresses
    double *p01 = p1 +  0;
    double *p11 = p1 +  8;
    double *p21 = p1 + 16;
    double *p31 = p1 + 24;
    
    double *p02 = p2 +  0;
    double *p12 = p2 +  8;
    double *p22 = p2 + 16;
    double *p32 = p2 + 24;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m512d r01, r11, r21, r31, 
                r02, r12, r22, r32;
        
        // Main cycle unrolled by 4
        for ( ; i <= n2-32; i += 32 ) {
            
            r01 = _mm512_loadu_pd( p01 );
            r11 = _mm512_loadu_pd( p11 );
            r21 = _mm512_loadu_pd( p21 );
            r31 = _mm512_loadu_pd( p31 );
            
            r02 = _mm512_loadu_pd( p02 );
            r12 = _mm512_loadu_pd( p12 );
            r22 = _mm512_loadu_pd( p22 );
            r32 = _mm512_loadu_pd( p32 );
            
            _mm512_storeu_pd( p01, r02 );
            _mm512_storeu_pd( p11, r12 );
            _mm512_storeu_pd( p21, r22 );
            _mm512_storeu_pd( p31, r32 );
            
            _mm512_storeu_pd( p02, r01 );
            _mm512_storeu_pd( p12, r11 );
            _mm512_storeu_pd( p22, r21 );
            _mm512_storeu_pd( p32, r31 );
            
            p01 += 32;
            p11 += 32;
            p21 += 32;
            p31 += 32;
            
            p02 += 32;
            p12 += 32;
            p22 += 32;
            p32 += 32;
            
        }
        
        // Remainer loop
        for ( ; i <= n2-8; i += 8 ) {
            
            r01 = _mm512_loadu_pd( p01 );
            r02 = _mm512_loadu_pd( p02 );
            
            _mm512_storeu_pd( p01, r02 );
            _mm512_storeu_pd( p02, r01 );
            
            p01 += 8;
            p02 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        __m128d r01, r02;
        
        for ( ; i <= n2-2; i += 2 ) {
            
            r01 = _mm_loadu_pd( p01 );
            r02 = _mm_loadu_pd( p02 );
            
            _mm_storeu_pd( p01, r02 );
            _mm_storeu_pd( p02, r01 );
            
            p01 += 2;
            p02 += 2;
            
        }
        
    }
    
}
#endif