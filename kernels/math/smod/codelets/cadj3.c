#include <stddef.h>
#include <complex.h>
#include <emmintrin.h>
#include <immintrin.h>

extern inline __attribute__((always_inline))
void cadj3_carray_c( const int length,
                     const double *restrict fac,
                     const double complex *restrict arr_from,
                           double complex *restrict arr_to )

#if defined ( mem32 )
{
    
    // Casting memory addresses
    const double *pf = ( const double * ) arr_from;
          double *pt = (       double * ) arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m256d rfac = _mm256_broadcast_sd( fac );
        const __m256d radj = _mm256_set_pd( -0., 0., -0., 0. );
        
        // Registers to be used
        __m256d r00, r01, r02, r03,
                r04, r05, r06, r07;
        
        // Main cycle unrolled by 4 with fma instructions
        // if availaible
        for ( ; i <= length-8; i += 8 ) {
            
            r00 = _mm256_loadu_pd( pf +  0 );
            r01 = _mm256_loadu_pd( pf +  4 );
            r02 = _mm256_loadu_pd( pf +  8 );
            r03 = _mm256_loadu_pd( pf + 12 );
            
            r00 = _mm256_xor_pd( radj, r00 );
            r01 = _mm256_xor_pd( radj, r01 );
            r02 = _mm256_xor_pd( radj, r02 );
            r03 = _mm256_xor_pd( radj, r03 );
            
            r04 = _mm256_loadu_pd( pt +  0 );
            r05 = _mm256_loadu_pd( pt +  4 );
            r06 = _mm256_loadu_pd( pt +  8 );
            r07 = _mm256_loadu_pd( pt + 12 );
            
            #if defined (fma)
            r00 = _mm256_fmadd_pd( rfac, r00, r04 );
            r01 = _mm256_fmadd_pd( rfac, r01, r05 );
            r02 = _mm256_fmadd_pd( rfac, r02, r06 );
            r03 = _mm256_fmadd_pd( rfac, r03, r07 );
            #else
            r00 = _mm256_mul_pd( rfac, r00 );
            r01 = _mm256_mul_pd( rfac, r01 );
            r02 = _mm256_mul_pd( rfac, r02 );
            r03 = _mm256_mul_pd( rfac, r03 );
            
            r00 = _mm256_add_pd( r04, r00 );
            r01 = _mm256_add_pd( r05, r01 );
            r02 = _mm256_add_pd( r06, r02 );
            r03 = _mm256_add_pd( r07, r03 );
            #endif
            
            _mm256_storeu_pd( pt +  0, r00 );
            _mm256_storeu_pd( pt +  4, r01 );
            _mm256_storeu_pd( pt +  8, r02 );
            _mm256_storeu_pd( pt + 12, r03 );
            
            pf += 16;
            pt += 16;
            
        }
        
        // Remainder loop without fma as there is no chance
        // of hiding the latency
        for ( ; i <= length-2; i += 2 ) {
            
            r00 = _mm256_loadu_pd( pf );
            r04 = _mm256_loadu_pd( pt );
            
            r00 = _mm256_xor_pd( radj, r00 );
            r00 = _mm256_mul_pd( rfac, r00 );
            r00 = _mm256_add_pd( r00, r04 );
            
            _mm256_storeu_pd( pt, r00 );
            
            pf += 4;
            pt += 4;
            
        }
        
    }
    
    // Last SSE step if needed, againg, without fma
    if ( i < length ) {
        
        const __m128d rs00 = _mm_set_pd( -0., 0.);
        const __m128d rs01 = _mm_load1_pd( fac );
        
        __m128d rs02 = _mm_loadu_pd( pf );
        __m128d rs03 = _mm_loadu_pd( pt );
        
        rs02 = _mm_xor_pd( rs00, rs02 );
        rs02 = _mm_mul_pd( rs01, rs02 );
        rs02 = _mm_add_pd( rs02, rs03 );
        
        _mm_storeu_pd( pt, rs02 );
        
    }
    
}
#else
{
    
    // Casting memory addresses
    const double *pf = ( const double * ) arr_from;
          double *pt = (       double * ) arr_to;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m512d rfac = _mm512_set1_pd( *fac );
        const __m512d radj = _mm512_set_pd( -0., 0., -0., 0., -0., 0., -0., 0. );
        
        // Registers to be used
        __m512d r00, r01, r02, r03,
                r04, r05, r06, r07;
        
        // Main cycle unrolled by 4 with fma instructions
        // if availaible
        for ( ; i <= length-16; i += 16 ) {
            
            r00 = _mm512_loadu_pd( pf +  0 );
            r01 = _mm512_loadu_pd( pf +  8 );
            r02 = _mm512_loadu_pd( pf + 16 );
            r03 = _mm512_loadu_pd( pf + 24 );
            
            r00 = _mm512_xor_pd( radj, r00 );
            r01 = _mm512_xor_pd( radj, r01 );
            r02 = _mm512_xor_pd( radj, r02 );
            r03 = _mm512_xor_pd( radj, r03 );
            
            r04 = _mm512_loadu_pd( pt +  0 );
            r05 = _mm512_loadu_pd( pt +  8 );
            r06 = _mm512_loadu_pd( pt + 16 );
            r07 = _mm512_loadu_pd( pt + 24 );
            
            r00 = _mm512_fmadd_pd( rfac, r00, r04 );
            r01 = _mm512_fmadd_pd( rfac, r01, r05 );
            r02 = _mm512_fmadd_pd( rfac, r02, r06 );
            r03 = _mm512_fmadd_pd( rfac, r03, r07 );
            
            _mm512_storeu_pd( pt +  0, r00 );
            _mm512_storeu_pd( pt +  8, r01 );
            _mm512_storeu_pd( pt + 16, r02 );
            _mm512_storeu_pd( pt + 24, r03 );
            
            pf += 32;
            pt += 32;
            
        }
        
        // Remainder loop without fma as there is no chance
        // of hiding the latency
        for ( ; i <= length-4; i += 4 ) {
            
            r00 = _mm512_loadu_pd( pf );
            r04 = _mm512_loadu_pd( pt );
            
            r00 = _mm512_xor_pd( radj, r00 );
            r00 = _mm512_mul_pd( rfac, r00 );
            r00 = _mm512_add_pd( r00, r04 );
            
            _mm512_storeu_pd( pt, r00 );
            
            pf += 8;
            pt += 8;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < length ) {
        
        const __m128d rfac = _mm_load1_pd( fac );
        const __m128d radj = _mm_set_pd( -0., 0.);
        
        __m128d r00, r04;
        
        for ( ; i < length; i++ ) {
            
            r00 = _mm_loadu_pd( pf );
            r04 = _mm_loadu_pd( pt );
            
            r00 = _mm_xor_pd( radj, r00 );
            r00 = _mm_mul_pd( rfac, r00 );
            r00 = _mm_add_pd( r00, r04 );
            
            _mm_storeu_pd( pt, r00 );
            
            pf += 2;
            pt += 2;
            
        }
        
    }
    
}
#endif