#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void copy4_carray_c( const int length,
                     const double *restrict fac1,
                     const double *restrict fac2,
                     const double *restrict fac3,
                     const double complex *restrict arr1,
                     const double complex *restrict arr2,
                           double complex *restrict arr_to )

#if defined ( mem32 )
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    const double *p1 = ( const double * ) arr1;
    const double *p2 = ( const double * ) arr2;
          double *pt = (       double * ) arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m256d rfac1 = _mm256_broadcast_sd( fac1 );
        const __m256d rfac2 = _mm256_broadcast_sd( fac2 );
        const __m256d rfac3 = _mm256_broadcast_sd( fac3 );
        
        // Registers to be used
        __m256d r00, r01, r02, r03,
                r10, r11, r12, r13;
        
        // Main cycle unrolled by 4 with fma instructions
        for ( ; i <= n2-16; i += 16 ) {
            
            r00 = _mm256_loadu_pd( p1 +  0 );
            r01 = _mm256_loadu_pd( p1 +  4 );
            r10 = _mm256_loadu_pd( p1 +  8 );
            r11 = _mm256_loadu_pd( p1 + 12 );
            
            r00 = _mm256_mul_pd( rfac1, r00 );
            r01 = _mm256_mul_pd( rfac1, r01 );
            r10 = _mm256_mul_pd( rfac1, r10 );
            r11 = _mm256_mul_pd( rfac1, r11 );
            
            r02 = _mm256_loadu_pd( p2 +  0 );
            r03 = _mm256_loadu_pd( p2 +  4 );
            r12 = _mm256_loadu_pd( p2 +  8 );
            r13 = _mm256_loadu_pd( p2 + 12 );
            
            #if defined (fma)
            r00 = _mm256_fmadd_pd( rfac2, r02, r00 );
            r01 = _mm256_fmadd_pd( rfac2, r03, r01 );
            r10 = _mm256_fmadd_pd( rfac2, r12, r10 );
            r11 = _mm256_fmadd_pd( rfac2, r13, r11 );
            #else
            r02 = _mm256_mul_pd( rfac2, r02 );
            r03 = _mm256_mul_pd( rfac2, r03 );
            r12 = _mm256_mul_pd( rfac2, r12 );
            r13 = _mm256_mul_pd( rfac2, r13 );
            
            r00 = _mm256_add_pd( r02, r00 );
            r01 = _mm256_add_pd( r03, r01 );
            r10 = _mm256_add_pd( r12, r10 );
            r11 = _mm256_add_pd( r13, r11 );
            #endif
            
            r02 = _mm256_loadu_pd( pt +  0 );
            r03 = _mm256_loadu_pd( pt +  4 );
            r12 = _mm256_loadu_pd( pt +  8 );
            r13 = _mm256_loadu_pd( pt + 12 );
            
            #if defined (fma)
            r00 = _mm256_fmadd_pd( rfac3, r02, r00 );
            r01 = _mm256_fmadd_pd( rfac3, r03, r01 );
            r10 = _mm256_fmadd_pd( rfac3, r12, r10 );
            r11 = _mm256_fmadd_pd( rfac3, r13, r11 );
            #else
            r02 = _mm256_mul_pd( rfac3, r02 );
            r03 = _mm256_mul_pd( rfac3, r03 );
            r12 = _mm256_mul_pd( rfac3, r12 );
            r13 = _mm256_mul_pd( rfac3, r13 );
            
            r00 = _mm256_add_pd( r02, r00 );
            r01 = _mm256_add_pd( r03, r01 );
            r10 = _mm256_add_pd( r12, r10 );
            r11 = _mm256_add_pd( r13, r11 );
            #endif
            
            _mm256_storeu_pd( pt +  0, r00 );  
            _mm256_storeu_pd( pt +  4, r01 );
            _mm256_storeu_pd( pt +  8, r10 );  
            _mm256_storeu_pd( pt + 12, r11 );
            
            p1 += 16;
            p2 += 16;
            pt += 16;
            
        }
        
        // Remainder loop without fma instructions
        // as there is nowhere to hide their latency
        for ( ; i <= n2-4; i += 4 ) {
            
            r00 = _mm256_loadu_pd( p1 );
            r01 = _mm256_loadu_pd( p2 );
            r02 = _mm256_loadu_pd( pt );
            
            r00 = _mm256_mul_pd( rfac1, r00 );
            r01 = _mm256_mul_pd( rfac2, r01 );
            r02 = _mm256_mul_pd( rfac3, r02 );
            
            r02 = _mm256_add_pd( r02, r00 );
            r02 = _mm256_add_pd( r02, r01 );
            
            _mm256_storeu_pd( pt, r02 );
            
            p1 += 4;
            p2 += 4;
            pt += 4;
            
        }
        
    }
    
    // Last SSE step if needed, again, without fma
    if ( i <= n2-2 ) {
        
        const __m128d rfac1 = _mm_load1_pd( fac1 );
        const __m128d rfac2 = _mm_load1_pd( fac2 );
        const __m128d rfac3 = _mm_load1_pd( fac3 );
        
        __m128d r00, r01, r02;
        
        r00 = _mm_loadu_pd( p1 );
        r01 = _mm_loadu_pd( p2 );
        r02 = _mm_loadu_pd( pt );
        
        r00 = _mm_mul_pd( rfac1, r00 );
        r01 = _mm_mul_pd( rfac2, r01 );
        r02 = _mm_mul_pd( rfac3, r02 );
        
        r02 = _mm_add_pd( r02, r00 );
        r02 = _mm_add_pd( r02, r01 );
        
        _mm_storeu_pd( pt, r02 );
        
    }
    
}
#else
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    const double *p1 = ( const double * ) arr1;
    const double *p2 = ( const double * ) arr2;
          double *pt = (       double * ) arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m512d rfac1 = _mm512_set1_pd( *fac1 );
        const __m512d rfac2 = _mm512_set1_pd( *fac2 );
        const __m512d rfac3 = _mm512_set1_pd( *fac3 );
        
        // Registers to be used
        __m512d r00, r01, r02, r03,
                r10, r11, r12, r13;
        
        // Main cycle unrolled by 4 with fma instructions
        for ( ; i <= n2-32; i += 32 ) {
            
            r00 = _mm512_loadu_pd( p1 +  0 );
            r01 = _mm512_loadu_pd( p1 +  8 );
            r10 = _mm512_loadu_pd( p1 + 16 );
            r11 = _mm512_loadu_pd( p1 + 24 );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r01 = _mm512_mul_pd( rfac1, r01 );
            r10 = _mm512_mul_pd( rfac1, r10 );
            r11 = _mm512_mul_pd( rfac1, r11 );
            
            r02 = _mm512_loadu_pd( p2 +  0 );
            r03 = _mm512_loadu_pd( p2 +  8 );
            r12 = _mm512_loadu_pd( p2 + 16 );
            r13 = _mm512_loadu_pd( p2 + 24 );
            
            r00 = _mm512_fmadd_pd( rfac2, r02, r00 );
            r01 = _mm512_fmadd_pd( rfac2, r03, r01 );
            r10 = _mm512_fmadd_pd( rfac2, r12, r10 );
            r11 = _mm512_fmadd_pd( rfac2, r13, r11 );
            
            r02 = _mm512_loadu_pd( pt +  0 );
            r03 = _mm512_loadu_pd( pt +  8 );
            r12 = _mm512_loadu_pd( pt + 16 );
            r13 = _mm512_loadu_pd( pt + 24 );
            
            r00 = _mm512_fmadd_pd( rfac3, r02, r00 );
            r01 = _mm512_fmadd_pd( rfac3, r03, r01 );
            r10 = _mm512_fmadd_pd( rfac3, r12, r10 );
            r11 = _mm512_fmadd_pd( rfac3, r13, r11 );
            
            _mm512_storeu_pd( pt +  0, r00 );  
            _mm512_storeu_pd( pt +  8, r01 );
            _mm512_storeu_pd( pt + 16, r10 );  
            _mm512_storeu_pd( pt + 24, r11 );
            
            p1 += 32;
            p2 += 32;
            pt += 32;
            
        }
        
        // Remainder loop without fma instructions
        // as there is nowhere to hide their latency
        for ( ; i <= n2-8; i += 8 ) {
            
            r00 = _mm512_loadu_pd( p1 );
            r01 = _mm512_loadu_pd( p2 );
            r02 = _mm512_loadu_pd( pt );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r01 = _mm512_mul_pd( rfac2, r01 );
            r02 = _mm512_mul_pd( rfac3, r02 );
            
            r02 = _mm512_add_pd( r02, r00 );
            r02 = _mm512_add_pd( r02, r01 );
            
            _mm512_storeu_pd( pt, r02 );
            
            p1 += 8;
            p2 += 8;
            pt += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i <= n2-2 ) {
        
        const __m128d rfac1 = _mm_load1_pd( fac1 );
        const __m128d rfac2 = _mm_load1_pd( fac2 );
        const __m128d rfac3 = _mm_load1_pd( fac3 );
        
        __m128d r00, r01, r02;
        
        for ( ; i <= n2-2; i += 2 ) {
            
            r00 = _mm_loadu_pd( p1 );
            r01 = _mm_loadu_pd( p2 );
            r02 = _mm_loadu_pd( pt );
            
            r00 = _mm_mul_pd( rfac1, r00 );
            r01 = _mm_mul_pd( rfac2, r01 );
            r02 = _mm_mul_pd( rfac3, r02 );
            
            r02 = _mm_add_pd( r02, r00 );
            r02 = _mm_add_pd( r02, r01 );
            
            _mm_storeu_pd( pt, r02 );
            
            p1 += 2;
            p2 += 2;
            pt += 2;
            
        }
        
    }
    
}
#endif