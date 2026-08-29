#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void trans_carray_c( const int n,
                     const int length,
                     const double complex *restrict arr_from,
                           double complex *restrict arr_to )

{
    
    switch (n) {
        
        case 4:
        #if defined ( mem32 )
        {
            
            // Complex is two doubles
            const int n2 = 2 * length;
            
            // Casting memory references
            const double *pf = ( const double * ) arr_from;
                  double *pt = (       double * ) arr_to;
            
            // Memory references to be used
            double *pt0 = pt + 0 * n2;
            double *pt1 = pt + 1 * n2;
            double *pt2 = pt + 2 * n2;
            double *pt3 = pt + 3 * n2;
            
            // Iterator
            int i = 0;
            
            // Body of the cycle
            {
                
                // Registers to be used
                __m256d r0, r1, r2, r3, r4, r5;
                
                // Main cycle
                for ( ; i <= n2-4; i += 4 ) {
                    
                    r0 = _mm256_loadu_pd( pf +  0 );
                    r1 = _mm256_loadu_pd( pf +  4 );
                    r2 = _mm256_loadu_pd( pf +  8 );
                    r3 = _mm256_loadu_pd( pf + 12 );
                    
                    r4 = _mm256_permute2f128_pd( r0, r2, 0x20 );
                    r5 = _mm256_permute2f128_pd( r1, r3, 0x20 );
                    
                    _mm256_storeu_pd( pt0, r4 );
                    _mm256_storeu_pd( pt2, r5 );
                    
                    r0 = _mm256_permute2f128_pd( r0, r2, 0x31 );
                    r1 = _mm256_permute2f128_pd( r1, r3, 0x31 );
                    
                    _mm256_storeu_pd( pt1, r0 );
                    _mm256_storeu_pd( pt3, r1 );
                    
                    pf  += 16;
                    pt0 +=  4;
                    pt1 +=  4;
                    pt2 +=  4;
                    pt3 +=  4;
                    
                }
                
                
            }
            
            // SSE remainder (non-loop)
            if ( i <= n2-2 ) {
                
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
        #else
        {
            
            // Complex is two doubles
            const int n2 = 2 * length;
            
            // Casting memory references
            const double *pf = ( const double * ) arr_from;
                  double *pt = (       double * ) arr_to;
            
            // Memory references to be used
            double *pt0 = pt + 0 * n2;
            double *pt1 = pt + 1 * n2;
            double *pt2 = pt + 2 * n2;
            double *pt3 = pt + 3 * n2;
            
            // Iterator
            int i = 0;
            
            // Body of the cycle
            {
                
                // Registers to be used
                __m512d r0, r1, r2, r3, r4, r5, r6, r7;
                
                // Main AVX-512 cycle
                for ( ; i <= n2-8; i += 8 ) {
                    
                    r0 = _mm512_loadu_pd( pf +  0 );
                    r1 = _mm512_loadu_pd( pf +  8 );
                    r2 = _mm512_loadu_pd( pf + 16 );
                    r3 = _mm512_loadu_pd( pf + 24 );
                    
                    r4 = _mm512_shuffle_f64x2( r0, r1, 0x44 );
                    r5 = _mm512_shuffle_f64x2( r0, r1, 0xEE );
                    r6 = _mm512_shuffle_f64x2( r2, r3, 0x44 );
                    r7 = _mm512_shuffle_f64x2( r2, r3, 0xEE );
                    
                    r0 = _mm512_shuffle_f64x2( r4, r6, 0x88 );
                    r1 = _mm512_shuffle_f64x2( r4, r6, 0xDD );
                    
                    _mm512_storeu_pd( pt0, r0 );
                    _mm512_storeu_pd( pt1, r1 );
                    
                    r2 = _mm512_shuffle_f64x2( r5, r7, 0x88 );
                    r3 = _mm512_shuffle_f64x2( r5, r7, 0xDD );
                    
                    _mm512_storeu_pd( pt2, r2 );
                    _mm512_storeu_pd( pt3, r3 );
                    
                    pf  += 32;
                    pt0 +=  8;
                    pt1 +=  8;
                    pt2 +=  8;
                    pt3 +=  8;
                    
                }
                
                
            }
            
            // AVX2 remainder (non-loop)
            if ( i <= n2-4 ) {
                
                __m256d r0, r1, r2, r3, r4, r5;
                
                r0 = _mm256_loadu_pd( pf +  0 );
                r1 = _mm256_loadu_pd( pf +  4 );
                r2 = _mm256_loadu_pd( pf +  8 );
                r3 = _mm256_loadu_pd( pf + 12 );
                
                r4 = _mm256_permute2f128_pd( r0, r2, 0x20 );
                r5 = _mm256_permute2f128_pd( r1, r3, 0x20 );
                
                _mm256_storeu_pd( pt0, r4 );
                _mm256_storeu_pd( pt2, r5 );
                
                r0 = _mm256_permute2f128_pd( r0, r2, 0x31 );
                r1 = _mm256_permute2f128_pd( r1, r3, 0x31 );
                
                _mm256_storeu_pd( pt1, r0 );
                _mm256_storeu_pd( pt3, r1 );
                
                pf  += 16;
                pt0 +=  4;
                pt1 +=  4;
                pt2 +=  4;
                pt3 +=  4;
                
                i += 4;
                
            }
            
            // SSE remainder (non-loop)
            if ( i <= n2-2 ) {
                
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
        #endif
        break;
        
        default:
        {
            
            // Memory references to be used
            double complex *restrict pt;
            const double complex *restrict pf;
            
            // Main cycle
            for ( int i2 = 0; i2 < n; i2++ ) {
                
                // Reference reset
                pt = arr_to + length * i2;
                pf = arr_from + i2;
                
                #pragma omp simd
                for ( int i1 = 0; i1 < length; i1++ ) {
                    
                    pt[i1] = pf[i1 * n]; 
                    
                }
                
            }
            
        }
        break;
        
    }
    
}