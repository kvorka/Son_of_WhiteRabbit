#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void fwd_idx3_c( const int n,
                 const double *restrict rcab,
                       double complex *restrict cab )

#if defined ( mem32 )
{
    
    // Casting memory references
    double *pcab = ( double * ) cab;
    
    // Memory references to be used
    const double *pr2 = rcab + 2*n;
          double *pc1 = pcab;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m256d r00, r01;
        
        // Main cycle unrolled by 2
        for ( i = 0; i <= n-4; i += 4 ) {
            
            r00 = _mm256_loadu_pd( pr2 + 0 );
            r01 = _mm256_loadu_pd( pr2 + 4 );
            
            _mm256_storeu_pd( pc1 + 0, r00 );
            _mm256_storeu_pd( pc1 + 4, r01 );
            
            pc1 += 8;
            pr2 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < n ) {
        
        for ( ; i < n; i++ ) {
            
            _mm_storeu_pd( pc1, _mm_loadu_pd( pr2 ) );
            
            pc1 += 2;
            pr2 += 2;
            
        }
        
    }
    
}
#else
{
    
    // Casting memory references
    double *pcab = ( double * ) cab;
    
    // Memory references to be used
    const double *pr2 = rcab + 2*n;
          double *pc1 = pcab;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Main cycle (no unroll due to small count)
        for ( i = 0; i <= n-4; i += 4 ) {
            
            _mm512_storeu_pd( pc1, _mm512_loadu_pd( pr2 ) );
            
            pc1 += 8;
            pr2 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < n ) {
        
        for ( ; i < n; i++ ) {
            
            _mm_storeu_pd( pc1, _mm_loadu_pd( pr2 ) );
            
            pc1 += 2;
            pr2 += 2;
            
        }
        
    }
    
}
#endif