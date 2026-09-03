#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void trans_4_carray_c( const int length,
                       const double complex *restrict arr_from,
                             double complex *restrict arr_to )

{
    
    // Casting memory references
    const double *restrict pf = ( const double * ) arr_from;
          double *restrict pt = (       double * ) arr_to;
    
    // Memory references to be used
    double *pt0 = pt + 0 * length;
    double *pt1 = pt + 2 * length;
    double *pt2 = pt + 4 * length;
    double *pt3 = pt + 6 * length;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m256d r0, r1, r2, r3, 
                r4, r5, r6, r7;
        
        // Main cycle
        for ( ; i <= length-2; i += 2 ) {
            
            r0 = _mm256_loadu_pd( pf +  0 );
            r1 = _mm256_loadu_pd( pf +  4 );
            r2 = _mm256_loadu_pd( pf +  8 );
            r3 = _mm256_loadu_pd( pf + 12 );
            
            r4 = _mm256_permute2f128_pd( r0, r2, 0x20 );
            r5 = _mm256_permute2f128_pd( r1, r3, 0x20 );
            r6 = _mm256_permute2f128_pd( r0, r2, 0x31 );
            r7 = _mm256_permute2f128_pd( r1, r3, 0x31 );
            
            _mm256_storeu_pd( pt0, r4 );
            _mm256_storeu_pd( pt2, r5 );
            _mm256_storeu_pd( pt1, r6 );
            _mm256_storeu_pd( pt3, r7 );
            
            pf  += 16;
            pt0 +=  4;
            pt1 +=  4;
            pt2 +=  4;
            pt3 +=  4;
            
        }
        
        
    }
    
    // SSE remainder (non-loop)
    if ( i < length ) {
        
        __m128d r0 = _mm_loadu_pd( pf + 0 );
        __m128d r1 = _mm_loadu_pd( pf + 2 );
        __m128d r2 = _mm_loadu_pd( pf + 4 );
        __m128d r3 = _mm_loadu_pd( pf + 6 );
        
        _mm_storeu_pd( pt0, r0 );
        _mm_storeu_pd( pt1, r1 );
        _mm_storeu_pd( pt2, r2 );
        _mm_storeu_pd( pt3, r3 );
        
    }
    
}