#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void trshf_3_carray_c( const int length,
                       const double complex *restrict v1,
                       const double complex *restrict v2,
                       const double complex *restrict v3,
                             double complex *restrict ca )

{
    
    // Casting memory addresses
    const double *pv1 = ( const double * ) v1;
    const double *pv2 = ( const double * ) v2;
    const double *pv3 = ( const double * ) v3;
          double *pt  = (       double * ) ca;
    
    // Memory addresses to be used
    const double *p1 = pv1 + 0*length;
    const double *p2 = pv1 + 2*length;
    const double *p3 = pv1 + 4*length;
    const double *p4 = pv2 + 0*length;
    const double *p5 = pv2 + 2*length;
    const double *p6 = pv2 + 4*length;
    const double *p7 = pv3 + 0*length;
    const double *p8 = pv3 + 2*length;
    const double *p9 = pv3 + 4*length;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m128d s00, s01;
        __m256d r00, r01, r02, r03,
                r04, r05, r06, r07;
        
        // Main cycle
        for ( ; i <= length-2; i += 2 ) {
            
            r00 = _mm256_loadu_pd( p1 );
            r01 = _mm256_loadu_pd( p4 );
            r04 = _mm256_loadu_pd( p7 );
            r05 = _mm256_loadu_pd( p2 );
            
            r02 = _mm256_permute2f128_pd( r00, r01, 0x20 );
            r03 = _mm256_permute2f128_pd( r00, r01, 0x31 );
            r06 = _mm256_permute2f128_pd( r04, r05, 0x20 );
            r07 = _mm256_permute2f128_pd( r04, r05, 0x31 );
            
            _mm256_storeu_pd( pt +  0, r02 );
            _mm256_storeu_pd( pt + 18, r03 );
            _mm256_storeu_pd( pt +  4, r06 );
            _mm256_storeu_pd( pt + 22, r07 );
            
            r00 = _mm256_loadu_pd( p5 );
            r01 = _mm256_loadu_pd( p8 );
            r04 = _mm256_loadu_pd( p3 );
            r05 = _mm256_loadu_pd( p6 );
            
            r02 = _mm256_permute2f128_pd( r00, r01, 0x20 );
            r03 = _mm256_permute2f128_pd( r00, r01, 0x31 );
            r06 = _mm256_permute2f128_pd( r04, r05, 0x20 );
            r07 = _mm256_permute2f128_pd( r04, r05, 0x31 );
            
            _mm256_storeu_pd( pt +  8, r02 );
            _mm256_storeu_pd( pt + 26, r03 );
            _mm256_storeu_pd( pt + 12, r06 );
            _mm256_storeu_pd( pt + 30, r07 );
            
            s00 = _mm_loadu_pd( p9 + 0 );
            s01 = _mm_loadu_pd( p9 + 2 );
            
            _mm_storeu_pd( pt + 16, s00 );
            _mm_storeu_pd( pt + 34, s01 );
            
            p1 +=  4;
            p2 +=  4;
            p3 +=  4;
            p4 +=  4;
            p5 +=  4;
            p6 +=  4;
            p7 +=  4;
            p8 +=  4;
            p9 +=  4;
            pt += 36;
            
        }
        
    }
    
    // SSE remainder (non-loop)
    if ( i < length ) {
            
        // Registers to be used
        __m128d s00, s01, s02;
        
        // Non-cycle remainder
        s00 = _mm_loadu_pd( p1 );
        s01 = _mm_loadu_pd( p4 );
        s02 = _mm_loadu_pd( p7 );
        
        _mm_storeu_pd( pt + 0, s00 );
        _mm_storeu_pd( pt + 2, s01 );
        _mm_storeu_pd( pt + 4, s02 );
        
        s00 = _mm_loadu_pd( p2 );
        s01 = _mm_loadu_pd( p5 );
        s02 = _mm_loadu_pd( p8 );
        
        _mm_storeu_pd( pt +  6, s00 );
        _mm_storeu_pd( pt +  8, s01 );
        _mm_storeu_pd( pt + 10, s02 );
        
        s00 = _mm_loadu_pd( p3 );
        s01 = _mm_loadu_pd( p6 );
        s02 = _mm_loadu_pd( p9 );
        
        _mm_storeu_pd( pt + 12, s00 );
        _mm_storeu_pd( pt + 14, s01 );
        _mm_storeu_pd( pt + 16, s02 );
        
    }
    
}