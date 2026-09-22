#include "../../../math/cvec.h"

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
    
    // Global constant registers
    const __td rsign = _t_setaltz_pd();
    const __td rfac1 = _t_set1_pd( *fac1 );
    const __td rfac4 = _t_set1_pd( *fac4 );
    
    // Registers to be used
    __td r00, r01, r02, r03,
         r10, r11, r12, r13;
    
    // curl1 and curl3 computation
    {
        
        // Iterator
        int i = 0;
        
        // Constant fac registers
        const __td rfac3 = _t_set1_pd( *fac3 );
        const __td rfac5 = _t_set1_pd( *fac5 );
        
        // Main loop unrolled by 4/8 complex numbers
        for ( ; i <= length-vlen; i += vlen ) {
            
            r00 = _t_loadu_pd( parr2  + vlen0 );
            r10 = _t_loadu_pd( parr2  + vlen1 );
            r01 = _t_loadu_pd( pdarr2 + vlen0 );
            r11 = _t_loadu_pd( pdarr2 + vlen1 );
            
            #if defined (__FMA__)
            r02 = _t_fmadd_pd(  rfac3, r00, r01 );
            r03 = _t_fnmadd_pd( rfac5, r00, r01 );
            r12 = _t_fmadd_pd(  rfac3, r10, r11 );
            r13 = _t_fnmadd_pd( rfac5, r10, r11 );
            #else
            r02 = _t_mul_pd( rfac3, r00 );
            r12 = _t_mul_pd( rfac3, r10 );
            r03 = _t_mul_pd( rfac5, r00 );
            r13 = _t_mul_pd( rfac5, r10 );
            
            r02 = _t_add_pd( r01, r02 );
            r12 = _t_add_pd( r11, r12 );
            r03 = _t_sub_pd( r01, r03 );
            r13 = _t_sub_pd( r11, r13 );
            #endif
            
            r02 = _t_mul_pd( rfac1, r02 );
            r12 = _t_mul_pd( rfac1, r12 );
            r03 = _t_mul_pd( rfac4, r03 );
            r13 = _t_mul_pd( rfac4, r13 );
            
            r02 = _t_permute_pd( r02, cmask );
            r12 = _t_permute_pd( r12, cmask );
            r03 = _t_permute_pd( r03, cmask );
            r13 = _t_permute_pd( r13, cmask );
            
            r02 = _t_xor_pd( r02, rsign );
            r12 = _t_xor_pd( r12, rsign );
            
            _t_storeu_pd( pcrl1 + vlen0, r02 );
            _t_storeu_pd( pcrl1 + vlen1, r12 );
            
            r03 = _t_xor_pd( r03, rsign );
            r13 = _t_xor_pd( r13, rsign );
            
            _t_storeu_pd( pcrl3 + vlen0, r03 );
            _t_storeu_pd( pcrl3 + vlen1, r13 );
            
            parr2  += vlen2;
            pdarr2 += vlen2;
            pcrl1  += vlen2;
            pcrl3  += vlen2;
            
        }
        
        // Non-loop remainder
        if ( i <= length-vlen/2 ) {
            
            r00 = _t_loadu_pd( parr2  );
            r01 = _t_loadu_pd( pdarr2 );
            
            #if defined (__FMA__)
            r02 = _t_fmadd_pd(  rfac3, r00, r01 );
            r03 = _t_fnmadd_pd( rfac5, r00, r01 );
            #else
            r02 = _t_mul_pd( rfac3, r00 );
            r03 = _t_mul_pd( rfac5, r00 );
            
            r02 = _t_add_pd( r01, r02 );
            r03 = _t_sub_pd( r01, r03 );
            #endif
            
            r02 = _t_mul_pd( rfac1, r02 );
            r03 = _t_mul_pd( rfac4, r03 );
            
            r02 = _t_permute_pd( r02, cmask );
            r03 = _t_permute_pd( r03, cmask );
            
            r02 = _t_xor_pd( r02, rsign );
            r03 = _t_xor_pd( r03, rsign );
            
            _t_storeu_pd( pcrl1, r02 );
            _t_storeu_pd( pcrl3, r03 );
            
            parr2  += vlen;
            pdarr2 += vlen;
            pcrl1  += vlen;
            pcrl3  += vlen;
            i      += vlen/2;
            
        }
        
        // Non-loop SSE/masked AVX512 remainder
        if ( i < length ) {
            
            #if !defined (__AVX512F__)
                
                // Constant register mapping
                const __m128d ssign = _mm256_castpd256_pd128( rsign );
                const __m128d sfac1 = _mm256_castpd256_pd128( rfac1 );
                const __m128d sfac3 = _mm256_castpd256_pd128( rfac3 );
                const __m128d sfac4 = _mm256_castpd256_pd128( rfac4 );
                const __m128d sfac5 = _mm256_castpd256_pd128( rfac5 );
                
                // Other registers to be used
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
            
            #else
                
                // Remainder mask
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
                
            #endif
            
        }
        
    }
    
    // curl2 computation
    {
        
        // Iterator
        int i = 0;
        
        // Constant registers
        const __td rfac2 = _t_set1_pd( *fac2 );
        const __td rfac6 = _t_set1_pd( *fac6 );
        
        // Main loop unrolled by 4/8 complex numbers
        for ( ; i <= length-vlen; i += vlen ) {
            
            r00 = _t_loadu_pd( parr1  + vlen0 );
            r10 = _t_loadu_pd( parr1  + vlen1 );
            r01 = _t_loadu_pd( pdarr1 + vlen0 );
            r11 = _t_loadu_pd( pdarr1 + vlen1 );
            
            #if defined (__FMA__)
            r00 = _t_fnmadd_pd( rfac2, r00, r01 );
            r10 = _t_fnmadd_pd( rfac2, r10, r11 );
            #else
            r00 = _t_mul_pd( rfac2, r00 );
            r10 = _t_mul_pd( rfac2, r10 );
            
            r00 = _t_sub_pd( r01, r00 );
            r10 = _t_sub_pd( r11, r10 );
            #endif
            
            r02 = _t_loadu_pd( parr3  + vlen0 );
            r12 = _t_loadu_pd( parr3  + vlen1 );
            r03 = _t_loadu_pd( pdarr3 + vlen0 );
            r13 = _t_loadu_pd( pdarr3 + vlen1 );
            
            #if defined (__FMA__)
            r02 = _t_fmadd_pd( rfac6, r02, r03 );
            r12 = _t_fmadd_pd( rfac6, r12, r13 );
            #else
            r02 = _t_mul_pd( rfac6, r02 );
            r12 = _t_mul_pd( rfac6, r12 );
            
            r02 = _t_add_pd( r03, r02 );
            r12 = _t_add_pd( r13, r12 );
            #endif
            
            r00 = _t_mul_pd( rfac1, r00 );
            r10 = _t_mul_pd( rfac1, r10 );
            r02 = _t_mul_pd( rfac4, r02 );
            r12 = _t_mul_pd( rfac4, r12 );
            
            r00 = _t_add_pd( r00, r02 );
            r10 = _t_add_pd( r10, r12 );
            
            r00 = _t_permute_pd( r00, cmask );
            r10 = _t_permute_pd( r10, cmask );
            
            r00 = _t_xor_pd( r00, rsign );
            r10 = _t_xor_pd( r10, rsign );
            
            _t_storeu_pd( pcrl2 + vlen0, r00 );
            _t_storeu_pd( pcrl2 + vlen1, r10 );
            
            parr1  += vlen2;
            pdarr1 += vlen2;
            parr3  += vlen2;
            pdarr3 += vlen2;
            pcrl2  += vlen2;
            
        }
        
        // Non-loop remainder
        if ( i <= length-vlen/2 ) {
            
            r00 = _t_loadu_pd( parr1  );
            r01 = _t_loadu_pd( pdarr1 );
            r02 = _t_loadu_pd( parr3  );
            r03 = _t_loadu_pd( pdarr3 );
            
            #if defined (__FMA__)
            r00 = _t_fnmadd_pd( rfac2, r00, r01 );
            r02 = _t_fmadd_pd(  rfac6, r02, r03 );
            #else
            r00 = _t_mul_pd( rfac2, r00 );
            r02 = _t_mul_pd( rfac6, r02 );
            
            r00 = _t_sub_pd( r01, r00 );
            r02 = _t_add_pd( r03, r02 );
            #endif
            
            r00 = _t_mul_pd( rfac1, r00 );
            r02 = _t_mul_pd( rfac4, r02 );
            
            r00 = _t_add_pd( r00, r02 );
            
            r00 = _t_permute_pd( r00, cmask );
            
            r00 = _t_xor_pd( r00, rsign );
            
            _t_storeu_pd( pcrl2, r00 );
            
            parr1  += vlen;
            pdarr1 += vlen;
            parr3  += vlen;
            pdarr3 += vlen;
            pcrl2  += vlen;
            i      += vlen/2;
            
        }
        
        // SSE/AVX512 masked remainders if needed
        if ( i < length ) {
            
            #if !defined (__AVX512F__)
                
                // Constant register mapping
                const __m128d ssign = _mm256_castpd256_pd128( rsign );
                const __m128d sfac1 = _mm256_castpd256_pd128( rfac1 );
                const __m128d sfac2 = _mm256_castpd256_pd128( rfac2 );
                const __m128d sfac4 = _mm256_castpd256_pd128( rfac4 );
                const __m128d sfac6 = _mm256_castpd256_pd128( rfac6 );
                
                // Other registers to be used
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
            
            #else
                
                // Mask for remainder
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
                
            #endif
            
        }
        
    }
    
}