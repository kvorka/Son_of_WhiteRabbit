#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void grad_pp_j_c( const int length,
                  const double *restrict fac1,
                  const double *restrict fac2,
                  const double *restrict fac3,
                  const double *restrict fac4,
                  const double complex *restrict darr,
                  const double complex *restrict arr,
                        double complex *restrict grad1,
                        double complex *restrict grad3 )

#if defined ( mem32 )
{
    
    // Casting memory addresses
    const double *pdarr = ( const double * ) darr;
    const double *parr  = ( const double * ) arr;
          double *pg1   = (       double * ) grad1;
          double *pg3   = (       double * ) grad3;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m256d rfac1 = _mm256_broadcast_sd( fac1 );
        const __m256d rfac2 = _mm256_broadcast_sd( fac2 );
        const __m256d rfac3 = _mm256_broadcast_sd( fac3 );
        const __m256d rfac4 = _mm256_broadcast_sd( fac4 );
        
        // Registers to be used
        __m256d r00, r01, r02, r03,
                r10, r11, r12, r13;
        
        // Main cycle unrolled by 2 with fma instructions
        for ( ; i <= length-4; i += 4 ) {
            
            r10 = _mm256_loadu_pd( parr  + 0 );
            r11 = _mm256_loadu_pd( parr  + 4 );
            r12 = _mm256_loadu_pd( pdarr + 0 );
            r13 = _mm256_loadu_pd( pdarr + 4 );
            
            #if defined ( fma )
            r00 = _mm256_fmadd_pd( rfac2, r10, r12 );
            r01 = _mm256_fmadd_pd( rfac2, r11, r13 );
            
            r10 = _mm256_fmadd_pd( rfac4, r10, r12 );
            r11 = _mm256_fmadd_pd( rfac4, r11, r13 );
            #else
            r00 = _mm256_mul_pd( rfac2, r10 );
            r01 = _mm256_mul_pd( rfac2, r11 );
            
            r10 = _mm256_mul_pd( rfac4, r10 );
            r11 = _mm256_mul_pd( rfac4, r11 );
            
            r00 = _mm256_add_pd( r12, r00 );
            r01 = _mm256_add_pd( r13, r01 );
            r10 = _mm256_add_pd( r12, r10 );
            r11 = _mm256_add_pd( r13, r11 );
            #endif
            
            r00 = _mm256_mul_pd( rfac1, r00 );
            r01 = _mm256_mul_pd( rfac1, r01 );
            r10 = _mm256_mul_pd( rfac3, r10 );
            r11 = _mm256_mul_pd( rfac3, r11 );
            
            _mm256_storeu_pd( pg1 + 0, r00 );
            _mm256_storeu_pd( pg1 + 4, r01 );
            _mm256_storeu_pd( pg3 + 0, r10 );
            _mm256_storeu_pd( pg3 + 4, r11 );
            
            pdarr += 8;
            parr  += 8;
            pg1   += 8;
            pg3   += 8;
            
        }
        
        // Remainder loop without fma instructions
        // as there is nowhere to hide their latency
        for ( ; i <= length-2; i += 2 ) {
            
            r10 = _mm256_loadu_pd( parr  );
            r12 = _mm256_loadu_pd( pdarr );
            
            r00 = _mm256_mul_pd( rfac2, r10 );
            r10 = _mm256_mul_pd( rfac4, r10 );
            
            r00 = _mm256_add_pd( r12, r00 );
            r10 = _mm256_add_pd( r12, r10 );
            
            r00 = _mm256_mul_pd( rfac1, r00 );
            r10 = _mm256_mul_pd( rfac3, r10 );
            
            _mm256_storeu_pd( pg1, r00 );
            _mm256_storeu_pd( pg3, r10 );
            
            pdarr += 4;
            parr  += 4;
            pg1   += 4;
            pg3   += 4;
            
        }
        
    }
    
    // Last SSE step if needed, again, without fma
    if ( i < length ) {
        
        const __m128d rfac1 = _mm_load1_pd( fac1 );
        const __m128d rfac2 = _mm_load1_pd( fac2 );
        const __m128d rfac3 = _mm_load1_pd( fac3 );
        const __m128d rfac4 = _mm_load1_pd( fac4 );
        
        __m128d r00, r10, r12;
        
        r10 = _mm_loadu_pd( parr  );
        r12 = _mm_loadu_pd( pdarr );
        
        r00 = _mm_mul_pd( rfac2, r10 );
        r10 = _mm_mul_pd( rfac4, r10 );
        
        r00 = _mm_add_pd( r12, r00 );
        r10 = _mm_add_pd( r12, r10 );
        
        r00 = _mm_mul_pd( rfac1, r00 );
        r10 = _mm_mul_pd( rfac3, r10 );
        
        _mm_storeu_pd( pg1, r00 );
        _mm_storeu_pd( pg3, r10 );
        
    }
    
}
#else
{
    
    // Casting memory addresses
    const double *pdarr = ( const double * ) darr;
    const double *parr  = ( const double * ) arr;
          double *pg1   = (       double * ) grad1;
          double *pg3   = (       double * ) grad3;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m512d rfac1 = _mm512_set1_pd( *fac1 );
        const __m512d rfac2 = _mm512_set1_pd( *fac2 );
        const __m512d rfac3 = _mm512_set1_pd( *fac3 );
        const __m512d rfac4 = _mm512_set1_pd( *fac4 );
        
        // Registers to be used
        __m512d r00, r01, r02, r03,
                r10, r11, r12, r13;
        
        // Main cycle unrolled by 2 with fma instructions
        for ( ; i <= length-8; i += 8 ) {
            
            r10 = _mm512_loadu_pd( parr  + 0 );
            r11 = _mm512_loadu_pd( parr  + 8 );
            r12 = _mm512_loadu_pd( pdarr + 0 );
            r13 = _mm512_loadu_pd( pdarr + 8 );
            
            r00 = _mm512_fmadd_pd( rfac2, r10, r12 );
            r01 = _mm512_fmadd_pd( rfac2, r11, r13 );
            
            r10 = _mm512_fmadd_pd( rfac4, r10, r12 );
            r11 = _mm512_fmadd_pd( rfac4, r11, r13 );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r01 = _mm512_mul_pd( rfac1, r01 );
            r10 = _mm512_mul_pd( rfac3, r10 );
            r11 = _mm512_mul_pd( rfac3, r11 );
            
            _mm512_storeu_pd( pg1 + 0, r00 );
            _mm512_storeu_pd( pg1 + 8, r01 );
            _mm512_storeu_pd( pg3 + 0, r10 );
            _mm512_storeu_pd( pg3 + 8, r11 );
            
            pdarr += 16;
            parr  += 16;
            pg1   += 16;
            pg3   += 16;
            
        }
        
        // Remainder loop without fma instructions
        // as there is nowhere to hide their latency
        for ( ; i <= length-4; i += 4 ) {
            
            r10 = _mm512_loadu_pd( parr  );
            r12 = _mm512_loadu_pd( pdarr );
            
            r00 = _mm512_mul_pd( rfac2, r10 );
            r10 = _mm512_mul_pd( rfac4, r10 );
            
            r00 = _mm512_add_pd( r12, r00 );
            r10 = _mm512_add_pd( r12, r10 );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r10 = _mm512_mul_pd( rfac3, r10 );
            
            _mm512_storeu_pd( pg1, r00 );
            _mm512_storeu_pd( pg3, r10 );
            
            pdarr += 8;
            parr  += 8;
            pg1   += 8;
            pg3   += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < length ) {
        
        const __m128d rfac1 = _mm_load1_pd( fac1 );
        const __m128d rfac2 = _mm_load1_pd( fac2 );
        const __m128d rfac3 = _mm_load1_pd( fac3 );
        const __m128d rfac4 = _mm_load1_pd( fac4 );
        
        __m128d r00, r10, r12;
        
        for ( ; i < length; i++ ) {
            
            r10 = _mm_loadu_pd( parr  );
            r12 = _mm_loadu_pd( pdarr );
            
            r00 = _mm_mul_pd( rfac2, r10 );
            r10 = _mm_mul_pd( rfac4, r10 );
            
            r00 = _mm_add_pd( r12, r00 );
            r10 = _mm_add_pd( r12, r10 );
            
            r00 = _mm_mul_pd( rfac1, r00 );
            r10 = _mm_mul_pd( rfac3, r10 );
            
            _mm_storeu_pd( pg1, r00 );
            _mm_storeu_pd( pg3, r10 );
            
            pdarr += 2;
            parr  += 2;
            pg1   += 2;
            pg3   += 2;
            
        }
        
    }
    
}
#endif