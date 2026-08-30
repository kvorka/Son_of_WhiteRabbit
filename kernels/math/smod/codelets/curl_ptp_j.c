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
    const double *pdarr1 = ( const double * ) darr1;
    const double *pdarr2 = ( const double * ) darr2;
    const double *pdarr3 = ( const double * ) darr3;
    const double *parr1  = ( const double * ) arr1;
    const double *parr2  = ( const double * ) arr2;
    const double *parr3  = ( const double * ) arr3;
          double *pcrl1  = (       double * ) curl1;
          double *pcrl2  = (       double * ) curl2;
          double *pcrl3  = (       double * ) curl3;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m256d rfac1 = _mm256_broadcast_sd( fac1 );
        const __m256d rfac2 = _mm256_broadcast_sd( fac2 );
        const __m256d rfac3 = _mm256_broadcast_sd( fac3 );
        const __m256d rfac4 = _mm256_broadcast_sd( fac4 );
        const __m256d rfac5 = _mm256_broadcast_sd( fac5 );
        const __m256d rfac6 = _mm256_broadcast_sd( fac6 );
        
        const __m256d rsign = _mm256_set_pd( 0., -0., 0., -0. );
        
        // Registers to be used
        __m256d r00, r01, r02, r03;
        
        // Main cycle with fma instructions
        for ( ; i <= length-2; i += 2 ) {
            
            r00 = _mm256_loadu_pd( parr2  );
            r01 = _mm256_loadu_pd( pdarr2 );
            
            #if defined ( fma )
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
            r03 = _mm256_xor_pd( r03, rsign );
            
            _mm256_storeu_pd( pcrl1, r02 );
            _mm256_storeu_pd( pcrl3, r03 );
            
            r00 = _mm256_loadu_pd( parr1  );
            r01 = _mm256_loadu_pd( pdarr1 );
            r02 = _mm256_loadu_pd( parr3  );
            r03 = _mm256_loadu_pd( pdarr3 );
            
            #if defined ( fma )
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
            parr2  += 4;
            pdarr2 += 4;
            parr3  += 4;
            pdarr3 += 4;
            
            pcrl1 += 4;
            pcrl2 += 4;
            pcrl3 += 4;
            
        }
        
    }
    
    // Last SSE step if needed
    if ( i < length ) {
        
        // Constant registers
        const __m128d rfac1 = _mm_load1_pd( fac1 );
        const __m128d rfac2 = _mm_load1_pd( fac2 );
        const __m128d rfac3 = _mm_load1_pd( fac3 );
        const __m128d rfac4 = _mm_load1_pd( fac4 );
        const __m128d rfac5 = _mm_load1_pd( fac5 );
        const __m128d rfac6 = _mm_load1_pd( fac6 );
        
        const __m128d rsign = _mm_set_pd( 0., -0. );
        
        __m128d r00, r01, r02, r03;
        
        r00 = _mm_loadu_pd( parr2  );
        r01 = _mm_loadu_pd( pdarr2 );
        
        #if defined ( fma )
        r02 = _mm_fmadd_pd(  rfac3, r00, r01 );
        r03 = _mm_fnmadd_pd( rfac5, r00, r01 );
        #else
        r02 = _mm_mul_pd( rfac3, r00 );
        r03 = _mm_mul_pd( rfac5, r00 );
        
        r02 = _mm_add_pd( r01, r02 );
        r03 = _mm_sub_pd( r01, r03 );
        #endif
        
        r02 = _mm_mul_pd( rfac1, r02 );
        r03 = _mm_mul_pd( rfac4, r03 );
        
        r02 = _mm_shuffle_pd( r02, r02, 1 );
        r03 = _mm_shuffle_pd( r03, r03, 1 );
        
        r02 = _mm_xor_pd( r02, rsign );
        r03 = _mm_xor_pd( r03, rsign );
        
        _mm_storeu_pd( pcrl1, r02 );
        _mm_storeu_pd( pcrl3, r03 );
        
        r00 = _mm_loadu_pd( parr1  );
        r01 = _mm_loadu_pd( pdarr1 );
        r02 = _mm_loadu_pd( parr3  );
        r03 = _mm_loadu_pd( pdarr3 );
        
        #if defined ( fma )
        r00 = _mm_fnmadd_pd( rfac2, r00, r01 );
        r02 = _mm_fmadd_pd(  rfac6, r02, r03 );
        #else
        r00 = _mm_mul_pd( rfac2, r00 );
        r02 = _mm_mul_pd( rfac6, r02 );
        
        r00 = _mm_sub_pd( r01, r00 );
        r02 = _mm_add_pd( r03, r02 );
        #endif
        
        r00 = _mm_mul_pd( rfac1, r00 );
        r02 = _mm_mul_pd( rfac4, r02 );
        
        r00 = _mm_add_pd( r00, r02 );
        
        r00 = _mm_shuffle_pd( r00, r00, 1 );
        
        r00 = _mm_xor_pd( r00, rsign );
        
        _mm_storeu_pd( pcrl2, r00 );
        
    }
    
}
#else
{
    
    // Casting memory addresses
    const double *pdarr1 = ( const double * ) darr1;
    const double *pdarr2 = ( const double * ) darr2;
    const double *pdarr3 = ( const double * ) darr3;
    const double *parr1  = ( const double * ) arr1;
    const double *parr2  = ( const double * ) arr2;
    const double *parr3  = ( const double * ) arr3;
          double *pcrl1  = (       double * ) curl1;
          double *pcrl2  = (       double * ) curl2;
          double *pcrl3  = (       double * ) curl3;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Constant registers
        const __m512d rfac1 = _mm512_set1_pd( *fac1 );
        const __m512d rfac2 = _mm512_set1_pd( *fac2 );
        const __m512d rfac3 = _mm512_set1_pd( *fac3 );
        const __m512d rfac4 = _mm512_set1_pd( *fac4 );
        const __m512d rfac5 = _mm512_set1_pd( *fac5 );
        const __m512d rfac6 = _mm512_set1_pd( *fac6 );
        
        const __m512d rsign = _mm512_set_pd( 0., -0., 0., -0., 0., -0., 0., -0. );
        
        // Registers to be used
        __m512d r00, r01, r02, r03,
                r10, r11, r12, r13;
        
        // Main cycle unrolled by 2 with fma instructions
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
            r03 = _mm512_mul_pd( rfac4, r03 );
            r12 = _mm512_mul_pd( rfac1, r12 );
            r13 = _mm512_mul_pd( rfac4, r13 );
            
            r02 = _mm512_permute_pd( r02, 0x55 );
            r03 = _mm512_permute_pd( r03, 0x55 );
            r12 = _mm512_permute_pd( r12, 0x55 );
            r13 = _mm512_permute_pd( r13, 0x55 );
            
            r02 = _mm512_xor_pd( r02, rsign );
            r03 = _mm512_xor_pd( r03, rsign );
            r12 = _mm512_xor_pd( r12, rsign );
            r13 = _mm512_xor_pd( r13, rsign );
            
            _mm512_storeu_pd( pcrl1 + 0, r02 );
            _mm512_storeu_pd( pcrl1 + 8, r12 );
            _mm512_storeu_pd( pcrl3 + 0, r03 );
            _mm512_storeu_pd( pcrl3 + 8, r13 );
            
            r00 = _mm512_loadu_pd( parr1  + 0 );
            r10 = _mm512_loadu_pd( parr1  + 8 );
            r01 = _mm512_loadu_pd( pdarr1 + 0 );
            r11 = _mm512_loadu_pd( pdarr1 + 8 );
            r02 = _mm512_loadu_pd( parr3  + 0 );
            r12 = _mm512_loadu_pd( parr3  + 8 );
            r03 = _mm512_loadu_pd( pdarr3 + 0 );
            r13 = _mm512_loadu_pd( pdarr3 + 8 );
            
            r00 = _mm512_fnmadd_pd( rfac2, r00, r01 );
            r02 = _mm512_fmadd_pd(  rfac6, r02, r03 );
            r10 = _mm512_fnmadd_pd( rfac2, r10, r11 );
            r12 = _mm512_fmadd_pd(  rfac6, r12, r13 );
            
            r00 = _mm512_mul_pd( rfac1, r00 );
            r02 = _mm512_mul_pd( rfac4, r02 );
            r10 = _mm512_mul_pd( rfac1, r10 );
            r12 = _mm512_mul_pd( rfac4, r12 );
            
            r00 = _mm512_add_pd( r00, r02 );
            r10 = _mm512_add_pd( r10, r12 );
            
            r00 = _mm512_permute_pd( r00, 0x55 );
            r10 = _mm512_permute_pd( r10, 0x55 );
            
            r00 = _mm512_xor_pd( r00, rsign );
            r10 = _mm512_xor_pd( r10, rsign );
            
            _mm512_storeu_pd( pcrl2 + 0, r00 );
            _mm512_storeu_pd( pcrl2 + 8, r10 );
            
            parr1  += 16;
            pdarr1 += 16;
            parr2  += 16;
            pdarr2 += 16;
            parr3  += 16;
            pdarr3 += 16;
            
            pcrl1 += 16;
            pcrl2 += 16;
            pcrl3 += 16;
            
        }
        
        // Remainder (non-loop)
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
            r03 = _mm512_xor_pd( r03, rsign );
            
            _mm512_storeu_pd( pcrl1, r02 );
            _mm512_storeu_pd( pcrl3, r03 );
            
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
            parr2  += 8;
            pdarr2 += 8;
            parr3  += 8;
            pdarr3 += 8;
            
            pcrl1 += 8;
            pcrl2 += 8;
            pcrl3 += 8;
            
            i += 4;
            
        }
        
    }
    
    // SSE remainder (could be split to avx/sse)
    if ( i < length ) {
        
        // Constant registers
        const __m128d rfac1 = _mm_load1_pd( fac1 );
        const __m128d rfac2 = _mm_load1_pd( fac2 );
        const __m128d rfac3 = _mm_load1_pd( fac3 );
        const __m128d rfac4 = _mm_load1_pd( fac4 );
        const __m128d rfac5 = _mm_load1_pd( fac5 );
        const __m128d rfac6 = _mm_load1_pd( fac6 );
        
        const __m128d rsign = _mm_set_pd( 0., -0. );
        
        __m128d r00, r01, r02, r03;
        
        for ( ; i < length; i++ ) {
            
            r00 = _mm_loadu_pd( parr2  );
            r01 = _mm_loadu_pd( pdarr2 );
            
            r02 = _mm_fmadd_pd(  rfac3, r00, r01 );
            r03 = _mm_fnmadd_pd( rfac5, r00, r01 );
            
            r02 = _mm_mul_pd( rfac1, r02 );
            r03 = _mm_mul_pd( rfac4, r03 );
            
            r02 = _mm_shuffle_pd( r02, r02, 1 );
            r03 = _mm_shuffle_pd( r03, r03, 1 );
            
            r02 = _mm_xor_pd( r02, rsign );
            r03 = _mm_xor_pd( r03, rsign );
            
            _mm_storeu_pd( pcrl1, r02 );
            _mm_storeu_pd( pcrl3, r03 );
            
            r00 = _mm_loadu_pd( parr1  );
            r01 = _mm_loadu_pd( pdarr1 );
            r02 = _mm_loadu_pd( parr3  );
            r03 = _mm_loadu_pd( pdarr3 );
            
            r00 = _mm_fnmadd_pd( rfac2, r00, r01 );
            r02 = _mm_fmadd_pd(  rfac6, r02, r03 );
            
            r00 = _mm_mul_pd( rfac1, r00 );
            r02 = _mm_mul_pd( rfac4, r02 );
            
            r00 = _mm_add_pd( r00, r02 );
            
            r00 = _mm_shuffle_pd( r00, r00, 1 );
            
            r00 = _mm_xor_pd( r00, rsign );
            
            _mm_storeu_pd( pcrl2, r00 );
            
            parr1  += 2;
            pdarr1 += 2;
            parr2  += 2;
            pdarr2 += 2;
            parr3  += 2;
            pdarr3 += 2;
            
            pcrl1 += 2;
            pcrl2 += 2;
            pcrl3 += 2;
            
        }
        
    }
    
}
#endif