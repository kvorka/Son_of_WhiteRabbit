#include <stddef.h>
#include <complex.h>
#include <emmintrin.h>

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
    const __m128d rsqrt2 = _mm_set1_pd( 0.7071067811865475 );
    const __m128d rsign  = _mm_set_pd( -0.0, 0.0 );
    
    // Registers to be used
    __m128d r1, r2, r3,
            r4, r5, r6;
    
    // Iterator
    int i = 0;
    
    // Main cycle unrolled by 2
    for ( ; i <= n-2; i+=2 ) {
        
        r1 = _mm_loadu_pd( psum1 );
        r2 = _mm_loadu_pd( psum2 );
        r3 = _mm_loadu_pd( psum3 );
        
        r4 = _mm_loadu_pd( psum1 + 2 );
        r5 = _mm_loadu_pd( psum2 + 2 );
        r6 = _mm_loadu_pd( psum3 + 2 );
        
        _mm_storeu_pd( pcc +  4, r2 );
        _mm_storeu_pd( pcc + 10, r5 );
        
        r2 = _mm_add_pd( r1, r3 );
        r5 = _mm_add_pd( r4, r6 );
        
        r1 = _mm_sub_pd( r1, r3 );
        r4 = _mm_sub_pd( r4, r6 );
        
        r1 = _mm_mul_pd( rsqrt2, r1 );
        r4 = _mm_mul_pd( rsqrt2, r4 );
        r2 = _mm_mul_pd( rsqrt2, r2 );
        r5 = _mm_mul_pd( rsqrt2, r5 );
        
        _mm_storeu_pd( pcc,     r1 );
        _mm_storeu_pd( pcc + 6, r4 );
        
        r2 = _mm_shuffle_pd( r2, r2, 1 );
        r5 = _mm_shuffle_pd( r5, r5, 1 );
        
        r2 = _mm_xor_pd( r2, rsign );
        r5 = _mm_xor_pd( r5, rsign );
        
        _mm_storeu_pd( pcc + 2,  r2 );
        _mm_storeu_pd( pcc + 8,  r5 );
        
        pcc += 12;
        
        psum1 += 4;
        psum2 += 4;
        psum3 += 4;
        
    }
    
    // Tail if needed
    if ( i < n ) {
        
        r1 = _mm_loadu_pd( psum1 );
        r2 = _mm_loadu_pd( psum2 );
        r3 = _mm_loadu_pd( psum3 );
        
        _mm_storeu_pd( pcc + 4, r2 );
        
        r2 = _mm_add_pd( r1, r3 );
        r1 = _mm_sub_pd( r1, r3 );
        
        r2 = _mm_mul_pd( rsqrt2, r2 );
        r1 = _mm_mul_pd( rsqrt2, r1 );
        
        _mm_storeu_pd( pcc, r1 );
        
        r2 = _mm_shuffle_pd( r2, r2, 1 );
        
        r2 = _mm_xor_pd( r2, rsign );
        
        _mm_storeu_pd( pcc + 2, r2 );
        
    }
    
}