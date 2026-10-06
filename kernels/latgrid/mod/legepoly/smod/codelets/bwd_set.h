#pragma once
#include "pmj_set.h"

static inline __attribute__((always_inline))
void bwd_set_c( const int n,
                const int m,
                const double *restrict cff,
                const double *restrict cosx,
                const double *restrict sinx,
                const double *restrict cc,
                      double *restrict pmm,
                      double *restrict pmj1,
                      double *restrict pmj,
                      double *restrict swork )

{   
    
    // Memory address of partial sums and coeffs
    const double *pcc = cc;
          double *psw = swork;
    
    // Registers to be used
    __td rccA, rccB,
         rpj0, rpj1, rpj2, rpj3,
         rs00, rs01, rs02, rs03,
         rs04, rs05, rs06, rs07;
    
    // Polynomials set-up
    pmj_set_c( m, cff, cosx, sinx, pmm, pmj1, pmj );
    
    // Load the polynomials and hope, that the compiler will
    // just rename the registers after inlining pmj_set_c
    rpj0 = _t_load_pd( pmj + vlen0 );
    rpj1 = _t_load_pd( pmj + vlen1 );
    rpj2 = _t_load_pd( pmj + vlen2 );
    rpj3 = _t_load_pd( pmj + vlen3 );
    
    // Loop over number of spectral rows
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        rccA = _t_set1_pd( *( pcc + 0 ) );
        rccB = _t_set1_pd( *( pcc + 1 ) );
        
        rs00 = _t_mul_pd( rpj0, rccA );
        rs01 = _t_mul_pd( rpj1, rccA );
        
        _t_store_pd( psw + vlen0, rs00 );
        _t_store_pd( psw + vlen1, rs01 );
        
        rs02 = _t_mul_pd( rpj2, rccA );
        rs03 = _t_mul_pd( rpj3, rccA );
        
        _t_store_pd( psw + vlen2, rs02 );
        _t_store_pd( psw + vlen3, rs03 );
        
        rs04 = _t_mul_pd( rpj0, rccB );
        rs05 = _t_mul_pd( rpj1, rccB );
        
        _t_store_pd( psw + vlen4, rs04 );
        _t_store_pd( psw + vlen5, rs05 );
        
        rs06 = _t_mul_pd( rpj2, rccB );
        rs07 = _t_mul_pd( rpj3, rccB );
        
        _t_store_pd( psw + vlen6, rs06 );
        _t_store_pd( psw + vlen7, rs07 );
        
        rccA = _t_set1_pd( *( pcc + 2 ) );
        rccB = _t_set1_pd( *( pcc + 3 ) );
        
        rs00 = _t_mul_pd( rpj0, rccA );
        rs01 = _t_mul_pd( rpj1, rccA );
        
        _t_store_pd( psw + vlen8, rs00 );
        _t_store_pd( psw + vlen9, rs01 );
        
        rs02 = _t_mul_pd( rpj2, rccA );
        rs03 = _t_mul_pd( rpj3, rccA );
        
        _t_store_pd( psw + vlen10, rs02 );
        _t_store_pd( psw + vlen11, rs03 );
        
        rs04 = _t_mul_pd( rpj0, rccB );
        rs05 = _t_mul_pd( rpj1, rccB );
        
        _t_store_pd( psw + vlen12, rs04 );
        _t_store_pd( psw + vlen13, rs05 );
        
        rs06 = _t_mul_pd( rpj2, rccB );
        rs07 = _t_mul_pd( rpj3, rccB );
        
        _t_store_pd( psw + vlen14, rs06 );
        _t_store_pd( psw + vlen15, rs07 );
        
        pcc += 4;
        psw += vlen16;
        
    }
    
}