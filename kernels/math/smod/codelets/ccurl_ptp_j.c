#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void curl_ptp_j_c( const int length,
                   const double *restrict fac1,
                   const double *restrict fac2,
                   const double *restrict fac3,
                   const double *restrict fac4,
                   const double *restrict fac5,
                   const double *restrict fac6,
                   const double complex *restrict darr1,
                   const double complex *restrict darr2,
                   const double complex *restrict darr3,
                   const double complex *restrict arr1,
                   const double complex *restrict arr2,
                   const double complex *restrict arr3,
                         double complex *restrict curl1,
                         double complex *restrict curl2,
                         double complex *restrict curl3 )

#if defined ( mem32 )
{
    
    // Casting memory addresses
    const double *restrict pdarr1 = ( const double * ) darr1;
    const double *restrict pdarr2 = ( const double * ) darr2;
    const double *restrict pdarr3 = ( const double * ) darr3;
    const double *restrict parr1  = ( const double * ) arr1;
    const double *restrict parr2  = ( const double * ) arr2;
    const double *restrict parr3  = ( const double * ) arr3;
          double *restrict pcrl1  = (       double * ) curl1;
          double *restrict pcrl2  = (       double * ) curl2;
          double *restrict pcrl3  = (       double * ) curl3;
    
    // For *cunit
    const __m256d rsign = _mm256_set_pd( 0., -0., 0., -0. );
    
    // Global constant fac registers
    const __m256d rfac1 = _mm256_broadcast_sd( fac1 );
    const __m256d rfac4 = _mm256_broadcast_sd( fac4 );
    
    // Registers to be used
    __m256d r00, r01, r02, r03,
            r10, r11, r12, r13;
    
    // curl1 and curl3 computation
    {
        
        // Iterator
        int i = 0;
        
        // Constant fac registers
        const __m256d rfac3 = _mm256_broadcast_sd( fac3 );
        const __m256d rfac5 = _mm256_broadcast_sd( fac5 );
        
        // Main loop unrolled by 4 complex numbers
        for ( ; i <= length-4; i += 4 ) {
            
            r00 = _mm256_loadu_pd( parr2  + 0 );
            r10 = _mm256_loadu_pd( parr2  + 4 );
            r01 = _mm256_loadu_pd( pdarr2 + 0 );
            r11 = _mm256_loadu_pd( pdarr2 + 4 );
            
            #if defined (__FMA__)
            r02 = _mm256_fmadd_pd(  rfac3, r00, r01 );
            r03 = _mm256_fnmadd_pd( rfac5, r00, r01 );
            r12 = _mm256_fmadd_pd(  rfac3, r10, r11 );
            r13 = _mm256_fnmadd_pd( rfac5, r10, r11 );
            #else
            r02 = _mm256_mul_pd( rfac3, r00 );
            r12 = _mm256_mul_pd( rfac3, r10 );
            r03 = _mm256_mul_pd( rfac5, r00 );
            r13 = _mm256_mul_pd( rfac5, r10 );
            
            r02 = _mm256_add_pd( r01, r02 );
            r12 = _mm256_add_pd( r11, r12 );
            r03 = _mm256_sub_pd( r01, r03 );
            r13 = _mm256_sub_pd( r11, r13 );
            #endif
            
            r02 = _mm256_mul_pd( rfac1, r02 );
            r12 = _mm256_mul_pd( rfac1, r12 );
            r03 = _mm256_mul_pd( rfac4, r03 );
            r13 = _mm256_mul_pd( rfac4, r13 );
            
            r02 = _mm256_permute_pd( r02, 0x05 );
            r12 = _mm256_permute_pd( r12, 0x05 );
            r03 = _mm256_permute_pd( r03, 0x05 );
            r13 = _mm256_permute_pd( r13, 0x05 );
            
            r02 = _mm256_xor_pd( r02, rsign );
            r12 = _mm256_xor_pd( r12, rsign );
            
            _mm256_storeu_pd( pcrl1 + 0, r02 );
            _mm256_storeu_pd( pcrl1 + 4, r12 );
            
            r03 = _mm256_xor_pd( r03, rsign );
            r13 = _mm256_xor_pd( r13, rsign );
            
            _mm256_storeu_pd( pcrl3 + 0, r03 );
            _mm256_storeu_pd( pcrl3 + 4, r13 );
            
            parr2  += 8;
            pdarr2 += 8;
            
            pcrl1 += 8;
            pcrl3 += 8;
            
        }
        
        // Non-loop remainder
        if ( i <= length-2 ) {
            
            r00 = _mm256_loadu_pd( parr2  );
            r01 = _mm256_loadu_pd( pdarr2 );
            
            #if defined (__FMA__)
            r02 = _mm256_fmadd_pd(  rfac3, r00, r01 );
            r03 = _mm256_fnmadd_pd( rfac5, r00, r01 );
            #else
            r02 = _mm256_mul_pd( rfac3, r00 );
            r03 = _mm256_mul_pd( rfac5, r00 );
            
            r02 = _mm256_add_pd( r01, r02 );
            r03 = _mm256_sub_pd( r01, r03 );
            #endif
            
            r02 = _mm256_mul_pd( rfac1, r02 );
            r03 = _mm256_mul_pd( rfac4, r03 );
            
            r02 = _mm256_permute_pd( r02, 0x05 );
            r03 = _mm256_permute_pd( r03, 0x05 );
            
            r02 = _mm256_xor_pd( r02, rsign );
            _mm256_storeu_pd( pcrl1, r02 );
            
            r03 = _mm256_xor_pd( r03, rsign );
            _mm256_storeu_pd( pcrl3, r03 );
            
            parr2  += 4;
            pdarr2 += 4;
            
            pcrl1 += 4;
            pcrl3 += 4;
            
            i += 2;
            
        }
        
        // Non-loop SSE remainder
        if ( i < length ) {
            
            const __m128d ssign = _mm256_castpd256_pd128( rsign );
            const __m128d sfac1 = _mm256_castpd256_pd128( rfac1 );
            const __m128d sfac3 = _mm256_castpd256_pd128( rfac3 );
            const __m128d sfac4 = _mm256_castpd256_pd128( rfac4 );
            const __m128d sfac5 = _mm256_castpd256_pd128( rfac5 );
            
            __m128d s00, s01, s02, s03;
            
            s00 = _mm_loadu_pd( parr2  );
            s01 = _mm_loadu_pd( pdarr2 );
            
            #if defined (__FMA__)
            s02 = _mm_fmadd_pd(  sfac3, s00, s01 );
            s03 = _mm_fnmadd_pd( sfac5, s00, s01 );
            #else
            s02 = _mm_mul_pd( sfac3, s00 );
            s03 = _mm_mul_pd( sfac5, s00 );
            
            s02 = _mm_add_pd( s01, s02 );
            s03 = _mm_sub_pd( s01, s03 );
            #endif
            
            s02 = _mm_mul_pd( sfac1, s02 );
            s03 = _mm_mul_pd( sfac4, s03 );
            
            s02 = _mm_shuffle_pd( s02, s02, 1 );
            s03 = _mm_shuffle_pd( s03, s03, 1 );
            
            s02 = _mm_xor_pd( s02, ssign );
            _mm_storeu_pd( pcrl1, s02 );
            
            s03 = _mm_xor_pd( s03, ssign );
            _mm_storeu_pd( pcrl3, s03 );
            
        }
        
    }
    
    // curl2 computation
    {
        
        // Iterator
        int i = 0;
        
        // Constant registers
        const __m256d rfac2 = _mm256_broadcast_sd( fac2 );
        const __m256d rfac6 = _mm256_broadcast_sd( fac6 );
        
        // Main loop unrolled by 4 complex numbers
        for ( ; i <= length-4; i += 4 ) {
            
            r00 = _mm256_loadu_pd( parr1  + 0 );
            r10 = _mm256_loadu_pd( parr1  + 4 );
            r01 = _mm256_loadu_pd( pdarr1 + 0 );
            r11 = _mm256_loadu_pd( pdarr1 + 4 );
            
            #if defined (__FMA__)
            r00 = _mm256_fnmadd_pd( rfac2, r00, r01 );
            r10 = _mm256_fnmadd_pd( rfac2, r10, r11 );
            #else
            r00 = _mm256_mul_pd( rfac2, r00 );
            r10 = _mm256_mul_pd( rfac2, r10 );
            
            r00 = _mm256_sub_pd( r01, r00 );
            r10 = _mm256_sub_pd( r11, r10 );
            #endif
            
            r02 = _mm256_loadu_pd( parr3  + 0 );
            r12 = _mm256_loadu_pd( parr3  + 4 );
            r03 = _mm256_loadu_pd( pdarr3 + 0 );
            r13 = _mm256_loadu_pd( pdarr3 + 4 );
            
            #if defined (__FMA__)
            r02 = _mm256_fmadd_pd(  rfac6, r02, r03 );
            r12 = _mm256_fmadd_pd(  rfac6, r12, r13 );
            #else
            r02 = _mm256_mul_pd( rfac6, r02 );
            r12 = _mm256_mul_pd( rfac6, r12 );
            
            r02 = _mm256_add_pd( r03, r02 );
            r12 = _mm256_add_pd( r13, r12 );
            #endif
            
            r00 = _mm256_mul_pd( rfac1, r00 );
            r10 = _mm256_mul_pd( rfac1, r10 );
            r02 = _mm256_mul_pd( rfac4, r02 );
            r12 = _mm256_mul_pd( rfac4, r12 );
            
            r00 = _mm256_add_pd( r00, r02 );
            r10 = _mm256_add_pd( r10, r12 );
            
            r00 = _mm256_permute_pd( r00, 0x05 );
            r10 = _mm256_permute_pd( r10, 0x05 );
            
            r00 = _mm256_xor_pd( r00, rsign );
            _mm256_storeu_pd( pcrl2 + 0, r00 );
            
            r10 = _mm256_xor_pd( r10, rsign );
            _mm256_storeu_pd( pcrl2 + 4, r10 );
            
            parr1  += 8;
            pdarr1 += 8;
            parr3  += 8;
            pdarr3 += 8;
            
            pcrl2 += 8;
            
        }
        
        // Non-loop remainder
        if ( i <= length-2 ) {
            
            r00 = _mm256_loadu_pd( parr1  );
            r01 = _mm256_loadu_pd( pdarr1 );
            r02 = _mm256_loadu_pd( parr3  );
            r03 = _mm256_loadu_pd( pdarr3 );
            
            #if defined (__FMA__)
            r00 = _mm256_fnmadd_pd( rfac2, r00, r01 );
            r02 = _mm256_fmadd_pd(  rfac6, r02, r03 );
            #else
            r00 = _mm256_mul_pd( rfac2, r00 );
            r02 = _mm256_mul_pd( rfac6, r02 );
            
            r00 = _mm256_sub_pd( r01, r00 );
            r02 = _mm256_add_pd( r03, r02 );
            #endif
            
            r00 = _mm256_mul_pd( rfac1, r00 );
            r02 = _mm256_mul_pd( rfac4, r02 );
            
            r00 = _mm256_add_pd( r00, r02 );
            
            r00 = _mm256_permute_pd( r00, 0x05 );
            r00 = _mm256_xor_pd( r00, rsign );
            _mm256_storeu_pd( pcrl2, r00 );
            
            parr1  += 4;
            pdarr1 += 4;
            parr3  += 4;
            pdarr3 += 4;
            
            pcrl2 += 4;
            
            i += 2;
            
        }
        
        // Non-loop SSE remainder
        if ( i < length ) {
            
            const __m128d ssign = _mm256_castpd256_pd128( rsign );
            const __m128d sfac1 = _mm256_castpd256_pd128( rfac1 );
            const __m128d sfac2 = _mm256_castpd256_pd128( rfac2 );
            const __m128d sfac4 = _mm256_castpd256_pd128( rfac4 );
            const __m128d sfac6 = _mm256_castpd256_pd128( rfac6 );
            
            __m128d s00, s01, s02, s03;
            
            s00 = _mm_loadu_pd( parr1  );
            s01 = _mm_loadu_pd( pdarr1 );
            s02 = _mm_loadu_pd( parr3  );
            s03 = _mm_loadu_pd( pdarr3 );
            
            #if defined (__FMA__)
            s00 = _mm_fnmadd_pd( sfac2, s00, s01 );
            s02 = _mm_fmadd_pd(  sfac6, s02, s03 );
            #else
            s00 = _mm_mul_pd( sfac2, s00 );
            s02 = _mm_mul_pd( sfac6, s02 );
            
            s00 = _mm_sub_pd( s01, s00 );
            s02 = _mm_add_pd( s03, s02 );
            #endif
            
            s00 = _mm_mul_pd( sfac1, s00 );
            s02 = _mm_mul_pd( sfac4, s02 );
            
            s00 = _mm_add_pd( s00, s02 );
            
            s00 = _mm_shuffle_pd( s00, s00, 1 );
            s00 = _mm_xor_pd( s00, ssign );
            _mm_storeu_pd( pcrl2, s00 );
            
        }
        
    }
    
}
#else
{
    
    // Casting memory addresses
    const double *restrict pdarr1 = ( const double * ) darr1;
    const double *restrict pdarr2 = ( const double * ) darr2;
    const double *restrict pdarr3 = ( const double * ) darr3;
    const double *restrict parr1  = ( const double * ) arr1;
    const double *restrict parr2  = ( const double * ) arr2;
    const double *restrict parr3  = ( const double * ) arr3;
          double *restrict pcrl1  = (       double * ) curl1;
          double *restrict pcrl2  = (       double * ) curl2;
          double *restrict pcrl3  = (       double * ) curl3;
    
    // For *cunit
    const __m512d rsign = _mm512_set_pd( 0., -0., 0., -0., 0., -0., 0., -0. );
    
    // Global constant fac registers
    const __m512d rfac1 = _mm512_set1_pd( *fac1 );
    const __m512d rfac4 = _mm512_set1_pd( *fac4 );
    
    // Registers to be used
    __m512d r00, r01, r02, r03,
            r10, r11, r12, r13;
    
    // curl1 and curl3 computation
    {
        
        // Iterator
        int i = 0;
        
        // Constant fac registers
        const __m512d rfac3 = _mm512_set1_pd( *fac3 );
        const __m512d rfac5 = _mm512_set1_pd( *fac5 );
        
        // Main loop unrolled by 8 complex numbers
        for ( ; i <= length-8; i += 8 ) {
            
            r00 = _mm512_loadu_pd( parr2  + 0 );
            r10 = _mm512_loadu_pd( parr2  + 8 );
            r01 = _mm512_loadu_pd( pdarr2 + 0 );
            r11 = _mm512_loadu_pd( pdarr2 + 8 );
            
            r02 = _mm512_fmadd_pd(  rfac3, r00, r01 );
            r03 = _mm512_fnmadd_pd( rfac5, r00, r01 );
            r12 = _mm512_fmadd_pd(  rfac3, r10, r11 );
            r13 = _mm512_fnmadd_pd( rfac5, r10, r11 );
            
            r02 = _mm512_mul_pd( rfac1, r02 );
            r12 = _mm512_mul_pd( rfac1, r12 );
            r03 = _mm512_mul_pd( rfac4, r03 );
            r13 = _mm512_mul_pd( rfac4, r13 );
            
            r02 = _mm512_permute_pd( r02, 0x55 );
            r12 = _mm512_permute_pd( r12, 0x55 );
            r03 = _mm512_permute_pd( r03, 0x55 );
            r13 = _mm512_permute_pd( r13, 0x55 );
            
            r02 = _mm512_xor_pd( r02, rsign );
            r12 = _mm512_xor_pd( r12, rsign );
            
            _mm512_storeu_pd( pcrl1 + 0, r02 );
            _mm512_storeu_pd( pcrl1 + 8, r12 );
            
            r03 = _mm512_xor_pd( r03, rsign );
            r13 = _mm512_xor_pd( r13, rsign );
            
            _mm512_storeu_pd( pcrl3 + 0, r03 );
            _mm512_storeu_pd( pcrl3 + 8, r13 );
            
            parr2  += 16;
            pdarr2 += 16;
            
            pcrl1 += 16;
            pcrl3 += 16;
            
        }
        
        // Non-loop remainder
        if ( i <= length-4 ) {
            
            r00 = _mm512_loadu_pd( parr2  );
            r01 = _mm512_loadu_pd( pdarr2 );
            
            r02 = _mm512_fmadd_pd(  rfac3, r00, r01 );
            r03 = _mm512_fnmadd_pd( rfac5, r00, r01 );
            
            r02 = _mm512_mul_pd( rfac1, r02 );
            r03 = _mm512_mul_pd( rfac4, r03 );
            
            r02 = _mm512_permute_pd( r02, 0x55 );
            r03 = _mm512_permute_pd( r03, 0x55 );
            
            r02 = _mm512_xor_pd( r02, rsign );
            _mm512_storeu_pd( pcrl1, r02 );
            
            r03 = _mm512_xor_pd( r03, rsign );
            _mm512_storeu_pd( pcrl3, r03 );
            
            parr2  += 8;
            pdarr2 += 8;
            
            pcrl1 += 8;
            pcrl3 += 8;
            
            i += 4;
            
        }
        
        // Non-loop masked remainder
        if ( i < length ) {
            
            const __mmask8 mask = _cvtu32_mask8( ( 1U << ( 2 * ( length - i ) ) ) - 1 );
            
            r00 = _mm512_maskz_loadu_pd( mask, parr2  );
            r01 = _mm512_maskz_loadu_pd( mask, pdarr2 );
            
            r02 = _mm512_fmadd_pd(  rfac3, r00, r01 );
            r03 = _mm512_fnmadd_pd( rfac5, r00, r01 );
            
            r02 = _mm512_mul_pd( rfac1, r02 );
            r03 = _mm512_mul_pd( rfac4, r03 );
            
            r02 = _mm512_permute_pd( r02, 0x55 );
            r03 = _mm512_permute_pd( r03, 0x55 );
            
            r02 = _mm512_xor_pd( r02, rsign );
            _mm512_mask_storeu_pd( pcrl1, mask, r02 );
            
            r03 = _mm512_xor_pd( r03, rsign );
            _mm512_mask_storeu_pd( pcrl3, mask, r03 );
            
        }
        
    }
    
    // curl2 computation
    {
        
        // Iterator
        int i = 0;
        
        // Constant registers
        const __m512d rfac2 = _mm512_set1_pd( *fac2 );
        const __m512d rfac6 = _mm512_set1_pd( *fac6 );
        
        // Main loop unrolled by 4 complex numbers
        for ( ; i <= length-8; i += 8 ) {
            
            r00 = _mm512_loadu_pd( parr1  + 0 );
            r10 = _mm512_loadu_pd( parr1  + 8 );
            r01 = _mm512_loadu_pd( pdarr1 + 0 );
            r11 = _mm512_loadu_pd( pdarr1 + 8 );
            
            r00 = _mm512_fnmadd_pd( rfac2, r00, r01 );
            r10 = _mm512_fnmadd_pd( rfac2, r10, r11 );
            
            r02 = _mm512_loadu_pd( parr3  + 0 );
            r12 = _mm512_loadu_pd( parr3  + 8 );
            r03 = _mm512_loadu_pd( pdarr3 + 0 );
            r13 = _mm512_loadu_pd( pdarr3 + 8 );
            
            r02 = _mm512_fmadd_pd( rfac6, r02, r03 );
            r12 = _mm512_fmadd_pd( rfac6, r12, r13 );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r10 = _mm512_mul_pd( rfac1, r10 );
            r02 = _mm512_mul_pd( rfac4, r02 );
            r12 = _mm512_mul_pd( rfac4, r12 );
            
            r00 = _mm512_add_pd( r00, r02 );
            r10 = _mm512_add_pd( r10, r12 );
            
            r00 = _mm512_permute_pd( r00, 0x55 );
            r10 = _mm512_permute_pd( r10, 0x55 );
            
            r00 = _mm512_xor_pd( r00, rsign );
            _mm512_storeu_pd( pcrl2 + 0, r00 );
            
            r10 = _mm512_xor_pd( r10, rsign );
            _mm512_storeu_pd( pcrl2 + 8, r10 );
            
            parr1  += 16;
            pdarr1 += 16;
            parr3  += 16;
            pdarr3 += 16;
            
            pcrl2 += 16;
            
        }
        
        // Non-loop remainder
        if ( i <= length-4 ) {
            
            r00 = _mm512_loadu_pd( parr1  );
            r01 = _mm512_loadu_pd( pdarr1 );
            r02 = _mm512_loadu_pd( parr3  );
            r03 = _mm512_loadu_pd( pdarr3 );
            
            r00 = _mm512_fnmadd_pd( rfac2, r00, r01 );
            r02 = _mm512_fmadd_pd(  rfac6, r02, r03 );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r02 = _mm512_mul_pd( rfac4, r02 );
            
            r00 = _mm512_add_pd( r00, r02 );
            
            r00 = _mm512_permute_pd( r00, 0x55 );
            r00 = _mm512_xor_pd( r00, rsign );
            _mm512_storeu_pd( pcrl2, r00 );
            
            parr1  += 8;
            pdarr1 += 8;
            parr3  += 8;
            pdarr3 += 8;
            
            pcrl2 += 8;
            
            i += 4;
            
        }
        
        // Non-loop SSE remainder
        if ( i < length ) {
            
            const __mmask8 mask = _cvtu32_mask8( ( 1U << ( 2 * ( length - i ) ) ) - 1 );
            
            r00 = _mm512_maskz_loadu_pd( mask, parr1  );
            r01 = _mm512_maskz_loadu_pd( mask, pdarr1 );
            r02 = _mm512_maskz_loadu_pd( mask, parr3  );
            r03 = _mm512_maskz_loadu_pd( mask, pdarr3 );
            
            r00 = _mm512_fnmadd_pd( rfac2, r00, r01 );
            r02 = _mm512_fmadd_pd(  rfac6, r02, r03 );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r02 = _mm512_mul_pd( rfac4, r02 );
            
            r00 = _mm512_add_pd( r00, r02 );
            
            r00 = _mm512_permute_pd( r00, 0x55 );
            r00 = _mm512_xor_pd( r00, rsign );
            _mm512_mask_storeu_pd( pcrl2, mask, r00 );
            
        }
        
    }
    
}
#endif