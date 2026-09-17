#include "../../../../../math/cvec.h"

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
    
    // pmm reccurence
    {
        
        r04 = _t_set1_pd( *cff );
        
        if ( ma1 == 1 ) {
            
            r00 = r04;
            r01 = r04;
            r02 = r04;
            r03 = r04;
            
        } else {
            
            r05 = _t_load_pd( sinx + 0*vlen );
            r06 = _t_load_pd( sinx + 1*vlen );
            r07 = _t_load_pd( sinx + 2*vlen );
            r08 = _t_load_pd( sinx + 3*vlen );
            
            r05 = _t_mul_pd( r04, r05 );
            r06 = _t_mul_pd( r04, r06 );
            r07 = _t_mul_pd( r04, r07 );
            r08 = _t_mul_pd( r04, r08 );
            
            r00 = _t_load_pd( pmm + 0*vlen );
            r01 = _t_load_pd( pmm + 1*vlen );
            r02 = _t_load_pd( pmm + 2*vlen );
            r03 = _t_load_pd( pmm + 3*vlen );
            
            r00 = _t_mul_pd( r05, r00 );
            r01 = _t_mul_pd( r06, r01 );
            r02 = _t_mul_pd( r07, r02 );
            r03 = _t_mul_pd( r08, r03 );
            
        }
        
        _t_store_pd( pmm + 0*vlen, r00 );
        _t_store_pd( pmm + 1*vlen, r01 );
        _t_store_pd( pmm + 2*vlen, r02 );
        _t_store_pd( pmm + 3*vlen, r03 );
        
    }
    
    // pmj1 and pmj set-up
    {
        
        r08 = _t_setzero_pd();
        
        _t_store_pd( pmj1 + 0*vlen, r08 );
        _t_store_pd( pmj1 + 1*vlen, r08 );
        _t_store_pd( pmj1 + 2*vlen, r08 );
        _t_store_pd( pmj1 + 3*vlen, r08 );
        
        r04 = _t_load_pd( cosx + 0*vlen );
        r05 = _t_load_pd( cosx + 1*vlen );
        r06 = _t_load_pd( cosx + 2*vlen );
        r07 = _t_load_pd( cosx + 3*vlen );
        
        r00 = _t_div_pd( r00, r04 );
        r01 = _t_div_pd( r01, r05 );
        r02 = _t_div_pd( r02, r06 );
        r03 = _t_div_pd( r03, r07 );
        
        _t_store_pd( pmj + 0*vlen, r00 );
        _t_store_pd( pmj + 1*vlen, r01 );
        _t_store_pd( pmj + 2*vlen, r02 );
        _t_store_pd( pmj + 3*vlen, r03 );
        
    }
    
    // Memory address of partial sums and coeffs
    double *pcr = cr;
    
    const double *psw0 = swork +  0*vlen;
    const double *psw1 = swork +  4*vlen;
    const double *psw2 = swork +  8*vlen;
    const double *psw3 = swork + 12*vlen;
    
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
        
        r08 = _t_load_pd( psw0 + 2*vlen );
        r09 = _t_load_pd( psw1 + 2*vlen );
        r10 = _t_load_pd( psw2 + 2*vlen );
        r11 = _t_load_pd( psw3 + 2*vlen );
        
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
        
        r08 = _t_load_pd( psw0 + 3*vlen );
        r09 = _t_load_pd( psw1 + 3*vlen );
        r10 = _t_load_pd( psw2 + 3*vlen );
        r11 = _t_load_pd( psw3 + 3*vlen );
        
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
        
        psw0 += 16*vlen;
        psw1 += 16*vlen;
        psw2 += 16*vlen;
        psw3 += 16*vlen;
        
        pcr += 4;
        
    }
    
}