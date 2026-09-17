#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void fwd_rec_c( const int n,
                const int nma,
                const double *restrict fmj,
                const double *restrict cosx2,
                const double *restrict swork,
                      double *restrict pmj1,
                      double *restrict pmj,
                      double *restrict cr )

{
    
    // Memory address of partial sums and coeffs
    double *pcr = cr;
    
    const double *psw0, *psw1, 
                 *psw2, *psw3;
    
    // Registers to be used, r00-r03 are reserved
    // for pmj values obtained from reccurrence
    __td r00, r01, r02, r03,
         r04, r05, r06, r07,
         r08, r09, r10, r11,
         r12, r13;
    
    // These are not really used, but help
    // to make the code cleaner
    __m256d reg0, reg1, reg2, reg3;
    
    // Cycle over weird degree iterator
    for ( int i4 = 0; i4 < nma; i4++ ) {
        
        // Reset partial sums references
        psw0 = swork +  0*vlen;
        psw1 = swork +  4*vlen;
        psw2 = swork +  8*vlen;
        psw3 = swork + 12*vlen;
        
        // Legendre polynomial reccurence
        {
            
            r00 = _t_load_pd( cosx2 + 0*vlen );
            r01 = _t_load_pd( cosx2 + 1*vlen );
            r02 = _t_load_pd( cosx2 + 2*vlen );
            r03 = _t_load_pd( cosx2 + 3*vlen );
            
            r04 = _t_set1_pd( *( fmj + 0 + 3*i4 ) );
            r05 = _t_set1_pd( *( fmj + 1 + 3*i4 ) );
            
            #if defined (__FMA__)
            r00 = _t_fmsub_pd( r04, r00, r05 );
            r01 = _t_fmsub_pd( r04, r01, r05 );
            r02 = _t_fmsub_pd( r04, r02, r05 );
            r03 = _t_fmsub_pd( r04, r03, r05 );
            #else
            r00 = _t_mul_pd( r04, r00 );
            r01 = _t_mul_pd( r04, r01 );
            r02 = _t_mul_pd( r04, r02 );
            r03 = _t_mul_pd( r04, r03 );
            
            r00 = _t_sub_pd( r00, r05 );
            r01 = _t_sub_pd( r01, r05 );
            r02 = _t_sub_pd( r02, r05 );
            r03 = _t_sub_pd( r03, r05 );
            #endif
            
            r04 = _t_load_pd( pmj1 + 0*vlen );
            r05 = _t_load_pd( pmj1 + 1*vlen );
            r06 = _t_load_pd( pmj1 + 2*vlen );
            r07 = _t_load_pd( pmj1 + 3*vlen );
            
            r08 = _t_set1_pd( *( fmj + 2 + 3*i4 ) );
            
            r04 = _t_mul_pd( r08, r04 );
            r05 = _t_mul_pd( r08, r05 );
            r06 = _t_mul_pd( r08, r06 );
            r07 = _t_mul_pd( r08, r07 );
            
            r08 = _t_load_pd( pmj + 0*vlen );
            r09 = _t_load_pd( pmj + 1*vlen );
            r10 = _t_load_pd( pmj + 2*vlen );
            r11 = _t_load_pd( pmj + 3*vlen );
            
            _t_store_pd( pmj1 + 0*vlen, r08 );
            _t_store_pd( pmj1 + 1*vlen, r09 );
            _t_store_pd( pmj1 + 2*vlen, r10 );
            _t_store_pd( pmj1 + 3*vlen, r11 );
            
            #if defined (__FMA__)
            r00 = _t_fmsub_pd( r00, r08, r04 );
            r01 = _t_fmsub_pd( r01, r09, r05 );
            r02 = _t_fmsub_pd( r02, r10, r06 );
            r03 = _t_fmsub_pd( r03, r11, r07 );
            #else
            r00 = _t_mul_pd( r00, r08);
            r01 = _t_mul_pd( r01, r09);
            r02 = _t_mul_pd( r02, r10);
            r03 = _t_mul_pd( r03, r11);
            
            r00 = _t_sub_pd( r00, r04 );
            r01 = _t_sub_pd( r01, r05 );
            r02 = _t_sub_pd( r02, r06 );
            r03 = _t_sub_pd( r03, r07 );
            #endif
            
            _t_store_pd( pmj + 0*vlen, r00 );
            _t_store_pd( pmj + 1*vlen, r01 );
            _t_store_pd( pmj + 2*vlen, r02 );
            _t_store_pd( pmj + 3*vlen, r03 );
            
        }
        
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
    
}