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
{
    
    // Complex is two doubles
    const int n2 = 2 * length;
    
    // Casting memory addresses
    const double *p1  = ( const double * ) arr1;
    const double *p2  = ( const double * ) arr2;
          double *pto = (       double * ) arr_to;
    
    // Other memory addresses
    const double *p10 = p1 + 0;
    const double *p11 = p1 + 4;
    
    const double *p20 = p2 + 0;
    const double *p21 = p2 + 4;
    
    double *pt0 = pto + 0;
    double *pt1 = pto + 4;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m256d rfac1 = _mm256_broadcast_sd( fac1 );
        const __m256d rfac2 = _mm256_broadcast_sd( fac2 );
        const __m256d rfac3 = _mm256_broadcast_sd( fac3 );
        
        // Registers to be used
        __m256d r10, r11,
                r20, r21,
                rt0, rt1;
        
        // Main cycle unrolled by 2 with fma instructions
        for ( ; i <= n2-8; i += 8 ) {
            
            r10 = _mm256_loadu_pd( p10 );
            r11 = _mm256_loadu_pd( p11 );
            
            r10 = _mm256_mul_pd( rfac1, r10 );
            r11 = _mm256_mul_pd( rfac1, r11 );
            
            r20 = _mm256_loadu_pd( p20 );
            r21 = _mm256_loadu_pd( p21 );
            
            #if defined (fma)
            r10 = _mm256_fmadd_pd( rfac2, r20, r10 );
            r11 = _mm256_fmadd_pd( rfac2, r21, r11 );
            #else
            r20 = _mm256_mul_pd( rfac2, r20 );
            r21 = _mm256_mul_pd( rfac2, r21 );
            
            r10 = _mm256_add_pd( r20, r10 );
            r11 = _mm256_add_pd( r21, r11 );
            #endif
            
            r20 = _mm256_loadu_pd( pt0 );
            r21 = _mm256_loadu_pd( pt1 );
            
            #if defined (fma)
            r10 = _mm256_fmadd_pd( rfac3, r20, r10 );
            r11 = _mm256_fmadd_pd( rfac3, r21, r11 );
            #else
            r20 = _mm256_mul_pd( rfac3, r20 );
            r21 = _mm256_mul_pd( rfac3, r21 );
            
            r10 = _mm256_add_pd( r20, r10 );
            r11 = _mm256_add_pd( r21, r11 );
            #endif
            
            _mm256_storeu_pd( pt0, r10 );  
            _mm256_storeu_pd( pt1, r11 );
            
            p10 += 8;
            p11 += 8;
            p20 += 8;
            p21 += 8;
            pt0 += 8;
            pt1 += 8;
            
        }
        
        // Remainer does not have to be looped, also, fma is dropped
        if ( i <= n2-4 ) {
            
            r10 = _mm256_loadu_pd( p10 );
            r20 = _mm256_loadu_pd( p20 );
            rt0 = _mm256_loadu_pd( pt0 );
            
            r10 = _mm256_mul_pd( rfac1, r10 );
            r20 = _mm256_mul_pd( rfac2, r20 );
            rt0 = _mm256_mul_pd( rfac3, rt0 );
            
            rt0 = _mm256_add_pd( rt0, r10 );
            
            rt0 = _mm256_add_pd( rt0, r20 );
            
            _mm256_storeu_pd( pt0, rt0 );
            
            p10 += 4;
            p20 += 4;
            pt0 += 4;
            
            i   += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i <= n2-2 ) {
        
        const __m128d rfac1 = _mm_load1_pd( fac1 );
        const __m128d rfac2 = _mm_load1_pd( fac2 );
        const __m128d rfac3 = _mm_load1_pd( fac3 );
        
        __m128d r10, r20, rt0;
        
        r10 = _mm_loadu_pd( p10 );
        r20 = _mm_loadu_pd( p20 );
        rt0 = _mm_loadu_pd( pt0 );
        
        r10 = _mm_mul_pd( rfac1, r10 );
        r20 = _mm_mul_pd( rfac2, r20 );
        rt0 = _mm_mul_pd( rfac3, rt0 );
        
        rt0 = _mm_add_pd( rt0, r10 );
        
        rt0 = _mm_add_pd( rt0, r20 );
        
        _mm_storeu_pd( pt0, rt0 );
        
    }
    
}