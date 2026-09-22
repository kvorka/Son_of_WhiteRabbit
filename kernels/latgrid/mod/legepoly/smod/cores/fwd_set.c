#include "pmj_set.h"

extern inline __attribute__((always_inline))
void fwd_set_c( const int n,
                const int ma1,
                const double *restrict cff,
                const double *restrict cosx,
                const double *restrict sinx,
                const double *restrict swork,
                      double *restrict pmm,
                      double *restrict pmj1,
                      double *restrict pmj,
                      double *restrict cr )

{
    
    // Registers to be used
    __td r00, r01, r02, r03,
         r04, r05, r06, r07,
         r08, r09, r10, r11;
    
    // These are not really used, but help
    // to make the code cleaner
    __m256d reg0, reg1, reg2, reg3;
    
    // Polynomials set-up
    pmj_set_c( ma1, cff, cosx, sinx, pmm, pmj1, pmj );
    
    // Load the polynomials and hope, that the compiler
    // will just rename the registers
    r00 = _t_load_pd( pmj + vlen0 );
    r01 = _t_load_pd( pmj + vlen1 );
    r02 = _t_load_pd( pmj + vlen2 );
    r03 = _t_load_pd( pmj + vlen3 );
    
    // Memory address of partial sums and coeffs
    double *pcr = cr;
    
    const double *psw0 = swork + vlen0;
    const double *psw1 = swork + vlen4;
    const double *psw2 = swork + vlen8;
    const double *psw3 = swork + vlen12;
    
    // Loop over number of spectral rows
    for ( int i2 = 0; i2 < n; i2++ ) {
        
        r04 = _t_load_pd( psw0 );
        r05 = _t_load_pd( psw1 );
        r06 = _t_load_pd( psw2 );
        r07 = _t_load_pd( psw3 );
        
        r04 = _t_mul_pd( r00, r04 );
        r05 = _t_mul_pd( r00, r05 );
        r06 = _t_mul_pd( r00, r06 );
        r07 = _t_mul_pd( r00, r07 );
        
        r08 = _t_load_pd( psw0 + vlen );
        r09 = _t_load_pd( psw1 + vlen );
        r10 = _t_load_pd( psw2 + vlen );
        r11 = _t_load_pd( psw3 + vlen );
        
        #if defined (__FMA__)
        r04 = _t_fmadd_pd( r01, r08, r04 );
        r05 = _t_fmadd_pd( r01, r09, r05 );
        r06 = _t_fmadd_pd( r01, r10, r06 );
        r07 = _t_fmadd_pd( r01, r11, r07 );
        #else
        r08 = _t_mul_pd( r01, r08 );
        r09 = _t_mul_pd( r01, r09 );
        r10 = _t_mul_pd( r01, r10 );
        r11 = _t_mul_pd( r01, r11 );
        
        r04 = _t_add_pd( r04, r08 );
        r05 = _t_add_pd( r05, r09 );
        r06 = _t_add_pd( r06, r10 );
        r07 = _t_add_pd( r07, r11 );
        #endif
        
        r08 = _t_load_pd( psw0 + vlen2 );
        r09 = _t_load_pd( psw1 + vlen2 );
        r10 = _t_load_pd( psw2 + vlen2 );
        r11 = _t_load_pd( psw3 + vlen2 );
        
        #if defined (__FMA__)
        r04 = _t_fmadd_pd( r02, r08, r04 );
        r05 = _t_fmadd_pd( r02, r09, r05 );
        r06 = _t_fmadd_pd( r02, r10, r06 );
        r07 = _t_fmadd_pd( r02, r11, r07 );
        #else
        r08 = _t_mul_pd( r02, r08 );
        r09 = _t_mul_pd( r02, r09 );
        r10 = _t_mul_pd( r02, r10 );
        r11 = _t_mul_pd( r02, r11 );
        
        r04 = _t_add_pd( r04, r08 );
        r05 = _t_add_pd( r05, r09 );
        r06 = _t_add_pd( r06, r10 );
        r07 = _t_add_pd( r07, r11 );
        #endif
        
        r08 = _t_load_pd( psw0 + vlen3 );
        r09 = _t_load_pd( psw1 + vlen3 );
        r10 = _t_load_pd( psw2 + vlen3 );
        r11 = _t_load_pd( psw3 + vlen3 );
        
        #if defined (__FMA__)
        r04 = _t_fmadd_pd( r03, r08, r04 );
        r05 = _t_fmadd_pd( r03, r09, r05 );
        r06 = _t_fmadd_pd( r03, r10, r06 );
        r07 = _t_fmadd_pd( r03, r11, r07 );
        #else
        r08 = _t_mul_pd( r03, r08 );
        r09 = _t_mul_pd( r03, r09 );
        r10 = _t_mul_pd( r03, r10 );
        r11 = _t_mul_pd( r03, r11 );
        
        r04 = _t_add_pd( r04, r08 );
        r05 = _t_add_pd( r05, r09 );
        r06 = _t_add_pd( r06, r10 );
        r07 = _t_add_pd( r07, r11 );
        #endif
        
        r08 = _t_unpacklo_pd( r04, r05 );
        r09 = _t_unpackhi_pd( r04, r05 );
        r10 = _t_unpacklo_pd( r06, r07 );
        r11 = _t_unpackhi_pd( r06, r07 );
        
        r08 = _t_add_pd( r08, r09 );
        r10 = _t_add_pd( r10, r11 );
        
        #if !defined (__AVX512F__)
            reg0 = r08;
            reg2 = r10;
        #else
            reg1 = _mm512_extractf64x4_pd( r08, 1 );
            reg3 = _mm512_extractf64x4_pd( r10, 1 );
            
            reg0 = _mm256_add_pd( _mm512_castpd512_pd256( r08 ), reg1 );
            reg2 = _mm256_add_pd( _mm512_castpd512_pd256( r10 ), reg3 );
        #endif
        
        reg1 = _mm256_permute2f128_pd( reg0, reg2, 0x31 );
        reg3 = _mm256_permute2f128_pd( reg0, reg2, 0x20 );
        
        reg0 = _mm256_add_pd( reg1, reg3 );
        reg2 = _mm256_loadu_pd( pcr );
        
        reg0 = _mm256_add_pd( reg0, reg2 );
        
        _mm256_storeu_pd( pcr, reg0 );
        
        pcr  += 4;
        psw0 += 4 * vlen4;
        psw1 += 4 * vlen4;
        psw2 += 4 * vlen4;
        psw3 += 4 * vlen4;
        
    }
    
}