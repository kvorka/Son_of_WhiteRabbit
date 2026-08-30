#include <stddef.h>
#include <complex.h>
#include <emmintrin.h>
#include <immintrin.h>

extern inline __attribute__((always_inline))
void eee2xyz_c( const int n,
                const double complex *restrict sumPTP,
                      double complex *restrict cc )
{
    
    // Casting memory addresses
    const double *psum = ( const double * ) sumPTP;
          double *pcc  = (       double * ) cc;
    
    // Addresses to be used (keep in mind complex == 2 doubles)
    const double *psum1 = psum + 0*n;
    const double *psum2 = psum + 2*n;
    const double *psum3 = psum + 4*n;
    
    // Register constants: 1 / sqrt(2) and sign
    const __m256d rsqrt2 = _mm256_set1_pd( 0.7071067811865475 );
    const __m256d rsign  = _mm256_set_pd( -0.0, 0.0, -0.0, 0.0 );
    
    // Iterator
    int i = 0;
    
    // Main cycle
    {
        
        // Registers to be used
        __m128d s1, s2;
        __m256d r01, r02, r03, r04, r05, r06,
                r11, r12, r13, r14;
        
        // Main cycle unrolled by 4
        for ( ; i <= n-4; i += 4 ) {
            
            r05 = _mm256_loadu_pd( psum2 + 0 );
            r06 = _mm256_loadu_pd( psum2 + 4 );
            
            _mm_storeu_pd( pcc +  4, _mm256_castpd256_pd128( r05 ) );
            _mm_storeu_pd( pcc + 16, _mm256_castpd256_pd128( r06 ) );
            
            s1 = _mm256_extractf128_pd( r05, 1 );
            s2 = _mm256_extractf128_pd( r06, 1 );
            
            r01 = _mm256_loadu_pd( psum1 + 0 );
            r02 = _mm256_loadu_pd( psum3 + 0 );
            r11 = _mm256_loadu_pd( psum1 + 4 );
            r12 = _mm256_loadu_pd( psum3 + 4 );
            
            _mm_storeu_pd( pcc + 10, s1 );
            _mm_storeu_pd( pcc + 22, s2 );
            
            r03 = _mm256_add_pd( r01, r02 );
            r04 = _mm256_sub_pd( r01, r02 );
            r13 = _mm256_add_pd( r11, r12 );
            r14 = _mm256_sub_pd( r11, r12 );
            
            r03 = _mm256_mul_pd( rsqrt2, r03 );
            r04 = _mm256_mul_pd( rsqrt2, r04 );
            r13 = _mm256_mul_pd( rsqrt2, r13 );
            r14 = _mm256_mul_pd( rsqrt2, r14 );
            
            r03 = _mm256_permute_pd( r03, 0x05 );
            r13 = _mm256_permute_pd( r13, 0x05 );
            
            r03 = _mm256_xor_pd( r03, rsign );
            r13 = _mm256_xor_pd( r13, rsign );
            
            r01 = _mm256_permute2f128_pd( r04, r03, 0x20);
            r02 = _mm256_permute2f128_pd( r04, r03, 0x31);
            r11 = _mm256_permute2f128_pd( r14, r13, 0x20);
            r12 = _mm256_permute2f128_pd( r14, r13, 0x31);
            
            _mm256_storeu_pd( pcc +  0, r01 );
            _mm256_storeu_pd( pcc +  6, r02 ) ;
            _mm256_storeu_pd( pcc + 12, r11 );
            _mm256_storeu_pd( pcc + 18, r12 ) ;
            
            pcc += 24;
            
            psum1 += 8;
            psum2 += 8;
            psum3 += 8;
            
        }
        
        // Remainder cycle
        for ( ; i <= n-2; i += 2 ) {
            
            r05 = _mm256_loadu_pd( psum2 );
            
            _mm_storeu_pd( pcc +  4, _mm256_castpd256_pd128( r05 ) );
            _mm_storeu_pd( pcc + 10, _mm256_extractf128_pd( r05, 1 ) );
            
            r01 = _mm256_loadu_pd( psum1 );
            r02 = _mm256_loadu_pd( psum3 );
            
            r03 = _mm256_add_pd( r01, r02 );
            r04 = _mm256_sub_pd( r01, r02 );
            
            r03 = _mm256_mul_pd( rsqrt2, r03 );
            r04 = _mm256_mul_pd( rsqrt2, r04 );
            
            r03 = _mm256_permute_pd( r03, 0x05 );
            
            r03 = _mm256_xor_pd( r03, rsign );
            
            r01 = _mm256_permute2f128_pd( r04, r03, 0x20);
            r02 = _mm256_permute2f128_pd( r04, r03, 0x31);
            
            _mm256_storeu_pd( pcc + 0, r01 );
            _mm256_storeu_pd( pcc + 6, r02 );
            
            pcc += 12;
            
            psum1 += 4;
            psum2 += 4;
            psum3 += 4;
            
        }
    
    }
    
    // Tail if needed
    if ( i < n ) {
        
        __m128d s1 = _mm_loadu_pd( psum1 );
        __m128d s2 = _mm_loadu_pd( psum2 );
        __m128d s3 = _mm_loadu_pd( psum3 );
        
        __m128d s4 = _mm_add_pd( s1, s3 );
        __m128d s5 = _mm_sub_pd( s1, s3 );
        
        __m128d ssq = _mm256_castpd256_pd128( rsqrt2 );
        
        s4 = _mm_mul_pd( ssq, s4 );
        s5 = _mm_mul_pd( ssq, s5 );
        
        _mm_storeu_pd( pcc + 0, s5 );
        _mm_storeu_pd( pcc + 4, s2 );
        
        s4 = _mm_shuffle_pd( s4, s4, 1 );
        s4 = _mm_xor_pd( s4, _mm256_castpd256_pd128( rsign ) );
        
        _mm_storeu_pd( pcc + 2, s4 );
        
    }
    
}