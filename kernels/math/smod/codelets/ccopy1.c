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
    
    // Complex is two doubles and unrolled increment
    const int n2 = 2 * length;
    
    // Casting memory addresses
    double *parr = ( double * ) arr;
    
    // Memory addresses to be used
    double *p0 = parr +  0;
    double *p1 = parr +  4;
    double *p2 = parr +  8;
    double *p3 = parr + 12;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant register
        const __m256d rfac = _mm256_broadcast_sd( fac );
        
        // Registers to be used
        __m256d r0, r1, r2, r3;
        
        for ( ; i <= n2-16; i += 16 ) {
            
            r0 = _mm256_loadu_pd( p0 );
            r1 = _mm256_loadu_pd( p1 );
            r2 = _mm256_loadu_pd( p2 );
            r3 = _mm256_loadu_pd( p3 );
            
            r0 = _mm256_mul_pd( rfac, r0 );
            r1 = _mm256_mul_pd( rfac, r1 );
            r2 = _mm256_mul_pd( rfac, r2 );
            r3 = _mm256_mul_pd( rfac, r3 );
            
            _mm256_storeu_pd( p0, r0 );
            _mm256_storeu_pd( p1, r1 );
            _mm256_storeu_pd( p2, r2 );
            _mm256_storeu_pd( p3, r3 );
            
            p0 += 16;
            p1 += 16;
            p2 += 16;
            p3 += 16;
            
        }
        
        // Remainder loop
        for ( ; i <= n2-4; i += 4 ) {
            
            _mm256_storeu_pd( p0, _mm256_mul_pd( rfac, _mm256_loadu_pd( p0 ) ) );
            
            p0 += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i <= n2-2 ) {
        
        _mm_storeu_pd( p0, _mm_mul_pd( _mm_load1_pd( fac ), _mm_loadu_pd( p0 ) ) );
        
    }
    
}
#else
{
    
    // Complex is two doubles and unrolled increment
    const int n2 = 2 * length;
    
    // Casting memory addresses
    double *parr = ( double * ) arr;
    
    // Memory addresses to be used
    double *p0 = parr +  0;
    double *p1 = parr +  8;
    double *p2 = parr + 16;
    double *p3 = parr + 24;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant register
        const __m512d rfac = _mm512_set1_pd( *fac );
        
        // Registers to be used
        __m512d r0, r1, r2, r3;
        
        for ( ; i <= n2-32; i += 32 ) {
            
            r0 = _mm512_loadu_pd( p0 );
            r1 = _mm512_loadu_pd( p1 );
            r2 = _mm512_loadu_pd( p2 );
            r3 = _mm512_loadu_pd( p3 );
            
            r0 = _mm512_mul_pd( rfac, r0 );
            r1 = _mm512_mul_pd( rfac, r1 );
            r2 = _mm512_mul_pd( rfac, r2 );
            r3 = _mm512_mul_pd( rfac, r3 );
            
            _mm512_storeu_pd( p0, r0 );
            _mm512_storeu_pd( p1, r1 );
            _mm512_storeu_pd( p2, r2 );
            _mm512_storeu_pd( p3, r3 );
            
            p0 += 32;
            p1 += 32;
            p2 += 32;
            p3 += 32;
            
        }
        
        // Remainder loop
        for ( ; i <= n2-8; i += 8 ) {
            
            _mm512_storeu_pd( p0, _mm512_mul_pd( rfac, _mm512_loadu_pd( p0 ) ) );
            
            p0 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
        
        for ( ; i <= n2-2; i += 2 ) {
            
            _mm_storeu_pd( p0, _mm_mul_pd( rfac, _mm_loadu_pd( p0 ) ) );
            
            p0 += 2;
            
        }
        
    }
    
}
#endif