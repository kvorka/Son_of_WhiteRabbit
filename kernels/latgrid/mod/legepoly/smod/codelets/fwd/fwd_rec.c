#include <immintrin.h>

extern inline __attribute__((always_inline))
void fwd_rec_c( const int n,
                const int nma,
                const double *restrict fmj,
                const double *restrict cosx2,
                const double *restrict swork,
                      double *restrict pmj1,
                      double *restrict pmj,
                      double *restrict cr )

#if defined ( mem32 )
{
    
    // Memory address of partial sums and coeffs
    double *pcr = cr;
    
    const double *psw0, *psw1, 
                 *psw2, *psw3;
    
    // Registers to be used, r00-r03 are reserved
    // for pmj values obtained from reccurrence
    __m256d r00, r01, r02, r03,
            r04, r05, r06, r07,
            r08, r09, r10, r11,
            r12, r13;
    
    // Cycle over weird degree iterator
    for ( int i4 = 0; i4 < nma; i4++ ) {
        
        // Reset partial sums references
        psw0 = swork +  0;
        psw1 = swork + 16;
        psw2 = swork + 32;
        psw3 = swork + 48;
        
        // Legendre polynomial reccurence
        {
            
            r00 = _mm256_load_pd( cosx2 +  0 );
            r01 = _mm256_load_pd( cosx2 +  4 );
            r02 = _mm256_load_pd( cosx2 +  8 );
            r03 = _mm256_load_pd( cosx2 + 12 );
            
            r04 = _mm256_broadcast_sd( fmj + 0 + 3*i4 );
            r05 = _mm256_broadcast_sd( fmj + 1 + 3*i4 );
            
            #if defined ( fma )
            r00 = _mm256_fmsub_pd( r04, r00, r05 );
            r01 = _mm256_fmsub_pd( r04, r01, r05 );
            r02 = _mm256_fmsub_pd( r04, r02, r05 );
            r03 = _mm256_fmsub_pd( r04, r03, r05 );
            #else
            r00 = _mm256_mul_pd( r04, r00 );
            r01 = _mm256_mul_pd( r04, r01 );
            r02 = _mm256_mul_pd( r04, r02 );
            r03 = _mm256_mul_pd( r04, r03 );
            
            r00 = _mm256_sub_pd( r00, r05 );
            r01 = _mm256_sub_pd( r01, r05 );
            r02 = _mm256_sub_pd( r02, r05 );
            r03 = _mm256_sub_pd( r03, r05 );
            #endif
            
            r04 = _mm256_load_pd( pmj1 +  0 );
            r05 = _mm256_load_pd( pmj1 +  4 );
            r06 = _mm256_load_pd( pmj1 +  8 );
            r07 = _mm256_load_pd( pmj1 + 12 );
            
            r08 = _mm256_broadcast_sd( fmj + 2 + 3*i4 );
            
            r04 = _mm256_mul_pd( r08, r04 );
            r05 = _mm256_mul_pd( r08, r05 );
            r06 = _mm256_mul_pd( r08, r06 );
            r07 = _mm256_mul_pd( r08, r07 );
            
            r08 = _mm256_load_pd( pmj +  0 );
            r09 = _mm256_load_pd( pmj +  4 );
            r10 = _mm256_load_pd( pmj +  8 );
            r11 = _mm256_load_pd( pmj + 12 );
            
            _mm256_store_pd( pmj1 +  0, r08 );
            _mm256_store_pd( pmj1 +  4, r09 );
            _mm256_store_pd( pmj1 +  8, r10 );
            _mm256_store_pd( pmj1 + 12, r11 );
            
            #if defined ( fma )
            r00 = _mm256_fmsub_pd( r00, r08, r04 );
            r01 = _mm256_fmsub_pd( r01, r09, r05 );
            r02 = _mm256_fmsub_pd( r02, r10, r06 );
            r03 = _mm256_fmsub_pd( r03, r11, r07 );
            #else
            r00 = _mm256_mul_pd( r00, r08);
            r01 = _mm256_mul_pd( r01, r09);
            r02 = _mm256_mul_pd( r02, r10);
            r03 = _mm256_mul_pd( r03, r11);
            
            r00 = _mm256_sub_pd( r00, r04 );
            r01 = _mm256_sub_pd( r01, r05 );
            r02 = _mm256_sub_pd( r02, r06 );
            r03 = _mm256_sub_pd( r03, r07 );
            #endif
            
            _mm256_store_pd( pmj +  0, r00 );
            _mm256_store_pd( pmj +  4, r01 );
            _mm256_store_pd( pmj +  8, r02 );
            _mm256_store_pd( pmj + 12, r03 );
            
        }
        
        // Loop over number of spectral rows
        for ( int i2 = 0; i2 < n; i2++ ) {
            
            r04 = _mm256_load_pd( psw0 );
            r05 = _mm256_load_pd( psw1 );
            r06 = _mm256_load_pd( psw2 );
            r07 = _mm256_load_pd( psw3 );
            
            r04 = _mm256_mul_pd( r00, r04 );
            r05 = _mm256_mul_pd( r00, r05 );
            r06 = _mm256_mul_pd( r00, r06 );
            r07 = _mm256_mul_pd( r00, r07 );
            
            r08 = _mm256_load_pd( psw0 + 4 );
            r09 = _mm256_load_pd( psw1 + 4 );
            r10 = _mm256_load_pd( psw2 + 4 );
            r11 = _mm256_load_pd( psw3 + 4 );
            
            #if defined (fma)
            r04 = _mm256_fmadd_pd( r01, r08, r04 );
            r05 = _mm256_fmadd_pd( r01, r09, r05 );
            r06 = _mm256_fmadd_pd( r01, r10, r06 );
            r07 = _mm256_fmadd_pd( r01, r11, r07 );
            #else
            r08 = _mm256_mul_pd( r01, r08 );
            r09 = _mm256_mul_pd( r01, r09 );
            r10 = _mm256_mul_pd( r01, r10 );
            r11 = _mm256_mul_pd( r01, r11 );
            
            r04 = _mm256_add_pd( r04, r08 );
            r05 = _mm256_add_pd( r05, r09 );
            r06 = _mm256_add_pd( r06, r10 );
            r07 = _mm256_add_pd( r07, r11 );
            #endif
            
            r08 = _mm256_load_pd( psw0 + 8 );
            r09 = _mm256_load_pd( psw1 + 8 );
            r10 = _mm256_load_pd( psw2 + 8 );
            r11 = _mm256_load_pd( psw3 + 8 );
            
            #if defined (fma)
            r04 = _mm256_fmadd_pd( r02, r08, r04 );
            r05 = _mm256_fmadd_pd( r02, r09, r05 );
            r06 = _mm256_fmadd_pd( r02, r10, r06 );
            r07 = _mm256_fmadd_pd( r02, r11, r07 );
            #else
            r08 = _mm256_mul_pd( r02, r08 );
            r09 = _mm256_mul_pd( r02, r09 );
            r10 = _mm256_mul_pd( r02, r10 );
            r11 = _mm256_mul_pd( r02, r11 );
            
            r04 = _mm256_add_pd( r04, r08 );
            r05 = _mm256_add_pd( r05, r09 );
            r06 = _mm256_add_pd( r06, r10 );
            r07 = _mm256_add_pd( r07, r11 );
            #endif
            
            r08 = _mm256_load_pd( psw0 + 12 );
            r09 = _mm256_load_pd( psw1 + 12 );
            r10 = _mm256_load_pd( psw2 + 12 );
            r11 = _mm256_load_pd( psw3 + 12 );
            
            #if defined (fma)
            r04 = _mm256_fmadd_pd( r03, r08, r04 );
            r05 = _mm256_fmadd_pd( r03, r09, r05 );
            r06 = _mm256_fmadd_pd( r03, r10, r06 );
            r07 = _mm256_fmadd_pd( r03, r11, r07 );
            #else
            r08 = _mm256_mul_pd( r03, r08 );
            r09 = _mm256_mul_pd( r03, r09 );
            r10 = _mm256_mul_pd( r03, r10 );
            r11 = _mm256_mul_pd( r03, r11 );
            
            r04 = _mm256_add_pd( r04, r08 );
            r05 = _mm256_add_pd( r05, r09 );
            r06 = _mm256_add_pd( r06, r10 );
            r07 = _mm256_add_pd( r07, r11 );
            #endif
            
            r08 = _mm256_unpacklo_pd( r04, r05 ); 
            r09 = _mm256_unpackhi_pd( r04, r05 );
            r10 = _mm256_unpacklo_pd( r06, r07 );
            r11 = _mm256_unpackhi_pd( r06, r07 );
            
            r08 = _mm256_add_pd( r08, r09 );
            r10 = _mm256_add_pd( r10, r11 );
            
            r07 = _mm256_permute2f128_pd( r08, r10, 0x31 );
            r09 = _mm256_permute2f128_pd( r08, r10, 0x20 );
            
            r07 = _mm256_add_pd( r07, r09 );
            r08 = _mm256_loadu_pd( pcr );
            
            r07 = _mm256_add_pd( r07, r08 );
            
            _mm256_storeu_pd( pcr, r07 );
            
            psw0 += 64;
            psw1 += 64;
            psw2 += 64;
            psw3 += 64;
            
            pcr += 4;
            
        }
        
    }
    
}
#else
{
    
    // Memory address of partial sums and coeffs
    double *pcr = cr;
    
    const double *psw0, *psw1, 
                 *psw2, *psw3;
    
    // Registers to be used, r00-r03 are reserved
    // for pmj values obtained from reccurrence
    __m512d r00, r01, r02, r03,
            r04, r05, r06, r07,
            r08, r09, r10, r11;
    
    __m256d reg0, reg1, reg2, reg3;
    
    // Cycle over weird degree iterator
    for ( int i4 = 0; i4 < nma; i4++ ) {
        
        // Reset partial sums references
        psw0 = swork +  0;
        psw1 = swork + 32;
        psw2 = swork + 64;
        psw3 = swork + 96;
        
        // Legendre polynomial reccurence
        {
            
            r00 = _mm512_load_pd( cosx2 +  0 );
            r01 = _mm512_load_pd( cosx2 +  8 );
            r02 = _mm512_load_pd( cosx2 + 16 );
            r03 = _mm512_load_pd( cosx2 + 24 );
            
            r04 = _mm512_set1_pd( *( fmj + 0 + 3*i4 ) );
            r05 = _mm512_set1_pd( *( fmj + 1 + 3*i4 ) );
            
            r00 = _mm512_fmsub_pd( r04, r00, r05 );
            r01 = _mm512_fmsub_pd( r04, r01, r05 );
            r02 = _mm512_fmsub_pd( r04, r02, r05 );
            r03 = _mm512_fmsub_pd( r04, r03, r05 );
            
            r04 = _mm512_load_pd( pmj1 +  0 );
            r05 = _mm512_load_pd( pmj1 +  8 );
            r06 = _mm512_load_pd( pmj1 + 16 );
            r07 = _mm512_load_pd( pmj1 + 24 );
            
            r08 = _mm512_set1_pd( *( fmj + 2 + 3*i4 ) );
            
            r04 = _mm512_mul_pd( r08, r04 );
            r05 = _mm512_mul_pd( r08, r05 );
            r06 = _mm512_mul_pd( r08, r06 );
            r07 = _mm512_mul_pd( r08, r07 );
            
            r08 = _mm512_load_pd( pmj +  0 );
            r09 = _mm512_load_pd( pmj +  8 );
            r10 = _mm512_load_pd( pmj + 16 );
            r11 = _mm512_load_pd( pmj + 24 );
            
            _mm512_store_pd( pmj1 +  0, r08 );
            _mm512_store_pd( pmj1 +  8, r09 );
            _mm512_store_pd( pmj1 + 16, r10 );
            _mm512_store_pd( pmj1 + 24, r11 );
            
            r00 = _mm512_fmsub_pd( r00, r08, r04 );
            r01 = _mm512_fmsub_pd( r01, r09, r05 );
            r02 = _mm512_fmsub_pd( r02, r10, r06 );
            r03 = _mm512_fmsub_pd( r03, r11, r07 );
            
            _mm512_store_pd( pmj +  0, r00 );
            _mm512_store_pd( pmj +  8, r01 );
            _mm512_store_pd( pmj + 16, r02 );
            _mm512_store_pd( pmj + 24, r03 );
            
        }
        
        // Loop over number of spectral rows
        for ( int i2 = 0; i2 < n; i2++ ) {
            
            r04 = _mm512_load_pd( psw0 + 0 );
            r05 = _mm512_load_pd( psw1 + 0 );
            r06 = _mm512_load_pd( psw2 + 0 );
            r07 = _mm512_load_pd( psw3 + 0 );
            
            r04 = _mm512_mul_pd( r00, r04 );
            r05 = _mm512_mul_pd( r00, r05 );
            r06 = _mm512_mul_pd( r00, r06 );
            r07 = _mm512_mul_pd( r00, r07 );
            
            r08 = _mm512_load_pd( psw0 + 8 );
            r09 = _mm512_load_pd( psw1 + 8 );
            r10 = _mm512_load_pd( psw2 + 8 );
            r11 = _mm512_load_pd( psw3 + 8 );
            
            r04 = _mm512_fmadd_pd( r01, r08, r04 );
            r05 = _mm512_fmadd_pd( r01, r09, r05 );
            r06 = _mm512_fmadd_pd( r01, r10, r06 );
            r07 = _mm512_fmadd_pd( r01, r11, r07 );
            
            r08 = _mm512_load_pd( psw0 + 16 );
            r09 = _mm512_load_pd( psw1 + 16 );
            r10 = _mm512_load_pd( psw2 + 16 );
            r11 = _mm512_load_pd( psw3 + 16 );
            
            r04 = _mm512_fmadd_pd( r02, r08, r04 );
            r05 = _mm512_fmadd_pd( r02, r09, r05 );
            r06 = _mm512_fmadd_pd( r02, r10, r06 );
            r07 = _mm512_fmadd_pd( r02, r11, r07 );
            
            r08 = _mm512_load_pd( psw0 + 24 );
            r09 = _mm512_load_pd( psw1 + 24 );
            r10 = _mm512_load_pd( psw2 + 24 );
            r11 = _mm512_load_pd( psw3 + 24 );
            
            r04 = _mm512_fmadd_pd( r03, r08, r04 );
            r05 = _mm512_fmadd_pd( r03, r09, r05 );
            r06 = _mm512_fmadd_pd( r03, r10, r06 );
            r07 = _mm512_fmadd_pd( r03, r11, r07 );
            
            r08 = _mm512_unpacklo_pd( r04, r05 ); 
            r09 = _mm512_unpackhi_pd( r04, r05 );
            r10 = _mm512_unpacklo_pd( r06, r07 );
            r11 = _mm512_unpackhi_pd( r06, r07 );
            
            r08 = _mm512_add_pd( r08, r09 );
            r10 = _mm512_add_pd( r10, r11 );
            
            reg0 = _mm512_castpd512_pd256( r08 );
            reg1 = _mm512_extractf64x4_pd( r08, 1 );
            reg2 = _mm512_castpd512_pd256( r10 );
            reg3 = _mm512_extractf64x4_pd( r10, 1 );
            
            reg0 = _mm256_add_pd( reg0, reg1 );
            reg2 = _mm256_add_pd( reg2, reg3 );
            
            reg1 = _mm256_permute2f128_pd( reg0, reg0, 0x01 );
            reg3 = _mm256_permute2f128_pd( reg2, reg2, 0x01 );
            
            reg0 = _mm256_add_pd( reg0, reg1 );
            reg2 = _mm256_add_pd( reg2, reg3 );
            
            reg1 = _mm256_loadu_pd( pcr );
            reg0 = _mm256_permute2f128_pd( reg0, reg2, 0x20 );
            
            reg0 = _mm256_add_pd( reg0, reg1 );
            
            _mm256_storeu_pd( pcr, reg0 );
            
            psw0 += 128;
            psw1 += 128;
            psw2 += 128;
            psw3 += 128;
            
            pcr += 4;
            
        }
        
    }
    
}
#endif