#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void bwd_idx1_c( const int n,
                 const double *restrict cff,
                 const double complex *restrict cab,
                       double *restrict rcab )

#if defined ( mem32 )
{
    
    // Casting memory references
    const double *pcab = ( const double * ) cab;
    
    // Memory references to be used
    const double *pc1 = pcab;
          double *pr1 = rcab;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m256d rfac1 = _mm256_broadcast_sd( cff );
        
        // Registers to be used
        __m256d r00, r01, r02, r03;
        
        // Main cycle unrolled by 4
        for ( i = 0; i <= n-8; i += 8 ) {
            
            r00 = _mm256_loadu_pd( pc1 +  0 );
            r01 = _mm256_loadu_pd( pc1 +  4 );
            r02 = _mm256_loadu_pd( pc1 +  8 );
            r03 = _mm256_loadu_pd( pc1 + 12 );
            
            r00 = _mm256_mul_pd( rfac1, r00 );
            r01 = _mm256_mul_pd( rfac1, r01 );
            r02 = _mm256_mul_pd( rfac1, r02 );
            r03 = _mm256_mul_pd( rfac1, r03 );
            
            _mm256_storeu_pd( pr1 +  0, r00 );
            _mm256_storeu_pd( pr1 +  4, r01 );
            _mm256_storeu_pd( pr1 +  8, r02 );
            _mm256_storeu_pd( pr1 + 12, r03 );
            
            pc1 += 16;
            pr1 += 16;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < n ) {
        
        // Constant registers
        const __m128d rfac1 = _mm_load1_pd( cff );
        
        // Main sse cycle
        for ( ; i < n; i++ ) {
            
            _mm_storeu_pd( pr1, _mm_mul_pd( rfac1, _mm_loadu_pd( pc1 ) ) );
            
            pc1 += 2;
            pr1 += 2;
            
        }
        
    }
    
}
#else
{
    
    // Casting memory references
    const double *pcab = ( const double * ) cab;
    
    // Memory references to be used
    const double *pc1 = pcab;
          double *pr1 = rcab;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m512d rfac1 = _mm512_set1_pd( *cff );
        
        // Registers to be used
        __m512d r00, r01;
        
        // Main cycle unrolled by 2
        for ( i = 0; i <= n-8; i += 8 ) {
            
            r00 = _mm512_loadu_pd( pc1 + 0 );
            r01 = _mm512_loadu_pd( pc1 + 8 );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r01 = _mm512_mul_pd( rfac1, r01 );
            
            _mm512_storeu_pd( pr1 + 0, r00 );
            _mm512_storeu_pd( pr1 + 8, r01 );
            
            pc1 += 16;
            pr1 += 16;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < n ) {
        
        // Constant registers
        const __m128d rfac1 = _mm_load1_pd( cff );
        
        // Main sse cycle
        for ( ; i < n; i++ ) {
            
            _mm_storeu_pd( pr1, _mm_mul_pd( rfac1, _mm_loadu_pd( pc1 ) ) );
            
            pc1 += 2;
            pr1 += 2;
            
        }
        
    }
    
}
#endif