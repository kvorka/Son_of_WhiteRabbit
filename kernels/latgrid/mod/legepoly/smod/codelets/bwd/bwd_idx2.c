#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void bwd_idx2_c( const int n,
                 const double *restrict cff,
                 const double complex *restrict cab,
                       double *restrict rcab )

#if defined ( mem32 )
{
    
    // Casting memory references
    const double *pcab = ( const double * ) cab;
    
    // Memory references to be used
    const double *pc1 = pcab + 0*n;
    const double *pc3 = pcab + 4*n;
          double *pr1 = rcab;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m256d rfac1 = _mm256_broadcast_sd( cff + 0 );
        const __m256d rfac2 = _mm256_broadcast_sd( cff + 1 );
        
        // Registers to be used
        __m256d r00, r01, r02, r03;
        
        // Main cycle unrolled by 2
        for ( i = 0; i <= n-4; i += 4 ) {
            
            r02 = _mm256_loadu_pd( pc1 + 0 );
            r03 = _mm256_loadu_pd( pc1 + 4 );
            r00 = _mm256_loadu_pd( pc3 + 0 );
            r01 = _mm256_loadu_pd( pc3 + 4 );
            
            r02 = _mm256_mul_pd( rfac1, r02 );
            r03 = _mm256_mul_pd( rfac1, r03 );
            
            #if defined ( fma )
            r00 = _mm256_fmadd_pd( rfac2, r00, r02 );
            r01 = _mm256_fmadd_pd( rfac2, r01, r03 );
            #else
            r00 = _mm256_mul_pd( rfac2, r00 );
            r01 = _mm256_mul_pd( rfac2, r01 );
            
            r00 = _mm256_add_pd( r00, r02 );
            r01 = _mm256_add_pd( r01, r03 );
            #endif
            
            _mm256_storeu_pd( pr1 + 0, r00 );
            _mm256_storeu_pd( pr1 + 4, r01 );
            
            pc1 += 8;
            pc3 += 8;
            pr1 += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < n ) {
        
        // Constant registers
        const __m128d rfac1 = _mm_load1_pd( cff + 0 );
        const __m128d rfac2 = _mm_load1_pd( cff + 1 );
        
        // Registers to be used
        __m128d r00, r02;
        
        for ( ; i < n; i++ ) {
            
            r00 = _mm_loadu_pd( pc1 );
            r02 = _mm_loadu_pd( pc3 );
            
            r00 = _mm_mul_pd( rfac1, r00 );
            r02 = _mm_mul_pd( rfac2, r02 );
            
            r00 = _mm_add_pd( r00, r02 );
            
            _mm_storeu_pd( pr1, r00 );
            
            pc1 += 2;
            pc3 += 2;
            pr1 += 2;
            
        }
        
    }
    
}
#else
{
    
    // Casting memory references
    const double *pcab = ( const double * ) cab;
    
    // Memory references to be used
    const double *pc1 = pcab + 0*n;
    const double *pc3 = pcab + 4*n;
          double *pr1 = rcab;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m512d rfac1 = _mm512_set1_pd( *( cff + 0 ) );
        const __m512d rfac2 = _mm512_set1_pd( *( cff + 1 ) );
        
        // Registers to be used
        __m512d r00, r01, r02, r03;
            
        // Main cycle unrolled by 2
        for ( i = 0; i <= n-8; i += 8 ) {
            
            r02 = _mm512_loadu_pd( pc1 + 0 );
            r03 = _mm512_loadu_pd( pc1 + 8 );
            r00 = _mm512_loadu_pd( pc3 + 0 );
            r01 = _mm512_loadu_pd( pc3 + 8 );
            
            r02 = _mm512_mul_pd( rfac1, r02 );
            r03 = _mm512_mul_pd( rfac1, r03 );
            
            #if defined ( fma )
            r00 = _mm512_fmadd_pd( rfac2, r00, r02 );
            r01 = _mm512_fmadd_pd( rfac2, r01, r03 );
            #else
            r00 = _mm512_mul_pd( rfac2, r00 );
            r01 = _mm512_mul_pd( rfac2, r01 );
            
            r00 = _mm512_add_pd( r00, r02 );
            r01 = _mm512_add_pd( r01, r03 );
            #endif
            
            _mm512_storeu_pd( pr1 + 0, r00 );
            _mm512_storeu_pd( pr1 + 8, r01 );
            
            pc1 += 16;
            pc3 += 16;
            pr1 += 16;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < n ) {
        
        // Constant registers
        const __m128d rfac1 = _mm_load1_pd( cff + 0 );
        const __m128d rfac2 = _mm_load1_pd( cff + 1 );
        
        // Registers to be used
        __m128d r00, r02;
        
        for ( ; i < n; i++ ) {
            
            r00 = _mm_loadu_pd( pc1 );
            r02 = _mm_loadu_pd( pc3 );
            
            r00 = _mm_mul_pd( rfac1, r00 );
            r02 = _mm_mul_pd( rfac2, r02 );
            
            r00 = _mm_add_pd( r00, r02 );
            
            _mm_storeu_pd( pr1, r00 );
            
            pc1 += 2;
            pc3 += 2;
            pr1 += 2;
            
        }
        
    }
    
}
#endif