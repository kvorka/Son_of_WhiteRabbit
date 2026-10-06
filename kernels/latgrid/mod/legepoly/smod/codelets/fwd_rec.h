#pragma once
#include "pmj_rec.h"

static inline __attribute__((always_inline))
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
    const double *restrict psw0; 
    const double *restrict psw1; 
    const double *restrict psw2;
    const double *restrict psw3;
          double *restrict pcr = cr;
    
    // Registers to be used
    __td rpj0, rpj1, rpj2, rpj3,
         rs00, rs01, rs02, rs03,
         rcr0, rcr1, rcr2, rcr3;
    
    // AVX512 finalizers
    #if defined (__AVX512F__)
    __m256d reg0, reg1, reg2, reg3;
    #endif
    
    // Cycle over weird degree iterator
    for ( int i4 = 0; i4 < nma; i4++ ) {
        
        // Reset partial sums references
        psw0 = swork + vlen0;
        psw1 = swork + vlen4;
        psw2 = swork + vlen8;
        psw3 = swork + vlen12;
        
        // Legendre polynomial reccurence
        pmj_rec_c( fmj+3*i4, cosx2, pmj1, pmj );
        
        // Load the polynomials and hope, that the compiler will
        // just rename the registers after inlining pmj_rec_c
        rpj0 = _t_load_pd( pmj + vlen0 );
        rpj1 = _t_load_pd( pmj + vlen1 );
        rpj2 = _t_load_pd( pmj + vlen2 );
        rpj3 = _t_load_pd( pmj + vlen3 );
        
        // Loop over number of spectral rows
        for ( int i2 = 0; i2 < n; i2++ ) {
            
            rs00 = _t_load_pd( psw0 );
            rs01 = _t_load_pd( psw1 );
            
            rcr0 = _t_mul_pd( rpj0, rs00 );
            rcr1 = _t_mul_pd( rpj0, rs01 );
            
            rs02 = _t_load_pd( psw2 );
            rs03 = _t_load_pd( psw3 );
            
            rcr2 = _t_mul_pd( rpj0, rs02 );
            rcr3 = _t_mul_pd( rpj0, rs03 );
            
            #if defined (__FMA__)
            
            rs00 = _t_load_pd( psw0 + vlen1 );
            rs01 = _t_load_pd( psw1 + vlen1 );
            
            rcr0 = _t_fmadd_pd( rpj1, rs00, rcr0 );
            rcr1 = _t_fmadd_pd( rpj1, rs01, rcr1 );
            
            rs02 = _t_load_pd( psw2 + vlen1 );
            rs03 = _t_load_pd( psw3 + vlen1 );
            
            rcr2 = _t_fmadd_pd( rpj1, rs02, rcr2 );
            rcr3 = _t_fmadd_pd( rpj1, rs03, rcr3 );
            
            rs00 = _t_load_pd( psw0 + vlen2 );
            rs01 = _t_load_pd( psw1 + vlen2 );
            
            rcr0 = _t_fmadd_pd( rpj2, rs00, rcr0 );
            rcr1 = _t_fmadd_pd( rpj2, rs01, rcr1 );
            
            rs02 = _t_load_pd( psw2 + vlen2 );
            rs03 = _t_load_pd( psw3 + vlen2 );
            
            rcr2 = _t_fmadd_pd( rpj2, rs02, rcr2 );
            rcr3 = _t_fmadd_pd( rpj2, rs03, rcr3 );
            
            rs00 = _t_load_pd( psw0 + vlen3 );
            rs01 = _t_load_pd( psw1 + vlen3 );
            
            rcr0 = _t_fmadd_pd( rpj3, rs00, rcr0 );
            rcr1 = _t_fmadd_pd( rpj3, rs01, rcr1 );
            
            rs00 = _t_unpacklo_pd( rcr0, rcr1 );
            rs01 = _t_unpackhi_pd( rcr0, rcr1 );
            
            rs02 = _t_load_pd( psw2 + vlen3 );
            rs03 = _t_load_pd( psw3 + vlen3 );
            
            rcr2 = _t_fmadd_pd( rpj3, rs02, rcr2 );
            rcr3 = _t_fmadd_pd( rpj3, rs03, rcr3 );
            
            rs02 = _t_unpacklo_pd( rcr2, rcr3 );
            rs03 = _t_unpackhi_pd( rcr2, rcr3 );
            
            #else
            
            rs00 = _t_load_pd( psw0 + vlen1 );
            rs01 = _t_load_pd( psw1 + vlen1 );
            
            rs00 = _t_mul_pd( rpj1, rs00 );
            rs01 = _t_mul_pd( rpj1, rs01 );
            
            rcr0 = _t_add_pd( rs00, rcr0 );
            rcr1 = _t_add_pd( rs01, rcr1 );
            
            rs02 = _t_load_pd( psw2 + vlen1 );
            rs03 = _t_load_pd( psw3 + vlen1 );
            
            rs02 = _t_mul_pd( rpj1, rs02 );
            rs03 = _t_mul_pd( rpj1, rs03 );
            
            rcr2 = _t_add_pd( rs02, rcr2 );
            rcr3 = _t_add_pd( rs03, rcr3 );
            
            rs00 = _t_load_pd( psw0 + vlen2 );
            rs01 = _t_load_pd( psw1 + vlen2 );
            
            rs00 = _t_mul_pd( rpj2, rs00 );
            rs01 = _t_mul_pd( rpj2, rs01 );
            
            rcr0 = _t_add_pd( rs00, rcr0 );
            rcr1 = _t_add_pd( rs01, rcr1 );
            
            rs02 = _t_load_pd( psw2 + vlen2 );
            rs03 = _t_load_pd( psw3 + vlen2 );
            
            rs02 = _t_mul_pd( rpj2, rs02 );
            rs03 = _t_mul_pd( rpj2, rs03 );
            
            rcr2 = _t_add_pd( rs02, rcr2 );
            rcr3 = _t_add_pd( rs03, rcr3 );
            
            rs00 = _t_load_pd( psw0 + vlen3 );
            rs01 = _t_load_pd( psw1 + vlen3 );
            
            rs00 = _t_mul_pd( rpj3, rs00 );
            rs01 = _t_mul_pd( rpj3, rs01 );
            
            rcr0 = _t_add_pd( rs00, rcr0 );
            rcr1 = _t_add_pd( rs01, rcr1 );
            
            rs00 = _t_unpacklo_pd( rcr0, rcr1 );
            rs01 = _t_unpackhi_pd( rcr0, rcr1 );
            
            rs02 = _t_load_pd( psw2 + vlen3 );
            rs03 = _t_load_pd( psw3 + vlen3 );
            
            rs02 = _t_mul_pd( rpj3, rs02 );
            rs03 = _t_mul_pd( rpj3, rs03 );
            
            rcr2 = _t_add_pd( rs02, rcr2 );
            rcr3 = _t_add_pd( rs03, rcr3 );
            
            rs02 = _t_unpacklo_pd( rcr2, rcr3 );
            rs03 = _t_unpackhi_pd( rcr2, rcr3 );
            
            #endif
            
            rs00 = _t_add_pd( rs00, rs01 );
            rs02 = _t_add_pd( rs02, rs03 );
            
            #if defined (__AVX512F__)
            
            reg1 = _mm512_extractf64x4_pd( rs00, 1 );
            reg3 = _mm512_extractf64x4_pd( rs02, 1 );
            
            reg0 = _mm256_add_pd( _mm512_castpd512_pd256( rs00 ), reg1 );
            reg2 = _mm256_add_pd( _mm512_castpd512_pd256( rs02 ), reg3 );
            
            reg1 = _mm256_permute2f128_pd( reg0, reg2, 0x31 );
            reg3 = _mm256_permute2f128_pd( reg0, reg2, 0x20 );
            
            reg0 = _mm256_add_pd( reg1, reg3 );
            reg2 = _mm256_loadu_pd( pcr );
            
            reg0 = _mm256_add_pd( reg0, reg2 );
            
            _mm256_storeu_pd( pcr, reg0 );
            
            #else
            
            rs01 = _mm256_permute2f128_pd( rs00, rs02, 0x31 );
            rs03 = _mm256_permute2f128_pd( rs00, rs02, 0x20 );
            
            rcr0 = _mm256_add_pd( rs01, rs03 );
            rcr1 = _mm256_loadu_pd( pcr );
            
            rcr0 = _mm256_add_pd( rcr0, rcr1 );
            
            _mm256_storeu_pd( pcr, rcr0 );
            
            #endif
            
            pcr  += 4;
            psw0 += vlen16;
            psw1 += vlen16;
            psw2 += vlen16;
            psw3 += vlen16;
            
        }
        
    }
    
}