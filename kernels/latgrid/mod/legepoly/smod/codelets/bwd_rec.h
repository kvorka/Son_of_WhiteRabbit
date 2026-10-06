#pragma once
#include "pmj_rec.h"

static inline __attribute__((always_inline))
void bwd_rec_c( const int n,
                const int nma,
                const double *restrict fmj,
                const double *restrict cosx2,
                const double *restrict cc,
                      double *restrict pmj1,
                      double *restrict pmj,
                      double *restrict swork )

{
    
    // Memory address of partial sums and coeffs
    const double *restrict pcc = cc;
          double *restrict psw;
    
    // Registers to be used
    __td rccA, rccB,
         rpj0, rpj1, rpj2, rpj3,
         rs00, rs01, rs02, rs03,
         rs04, rs05, rs06, rs07;
    
    // Cycle over weird degree iterator
    for ( int i4 = 0; i4 < nma; i4++ ) {
        
        // Reset the accumulator
        psw = swork;
        
        // Legendre polynomial reccurence
        pmj_rec_c( fmj+3*i4, cosx2, pmj1, pmj );
        
        // Load the polynomials and hope, that the compiler will
        // just rename the registers after inlining pmj_rec_c
        rpj0 = _t_load_pd( pmj + vlen0 );
        rpj1 = _t_load_pd( pmj + vlen1 );
        rpj2 = _t_load_pd( pmj + vlen2 );
        rpj3 = _t_load_pd( pmj + vlen3 );
        
        // Loop over number of spectral rows
        for ( int i3 = 0; i3 < n; i3++ ) {
            
            rccA = _t_set1_pd( *( pcc + 0 ) );
            rccB = _t_set1_pd( *( pcc + 1 ) );
            
            #if defined (__FMA__)
            
            rs00 = _t_load_pd( psw + vlen0 );
            rs01 = _t_load_pd( psw + vlen1 );
            
            rs00 = _t_fmadd_pd( rpj0, rccA, rs00 );
            rs01 = _t_fmadd_pd( rpj1, rccA, rs01 );
            
            _t_store_pd( psw + vlen0, rs00 );
            _t_store_pd( psw + vlen1, rs01 );
            
            rs02 = _t_load_pd( psw + vlen2 );
            rs03 = _t_load_pd( psw + vlen3 );
            
            rs02 = _t_fmadd_pd( rpj2, rccA, rs02 );
            rs03 = _t_fmadd_pd( rpj3, rccA, rs03 );
            
            _t_store_pd( psw + vlen2, rs02 );
            _t_store_pd( psw + vlen3, rs03 );
            
            rs04 = _t_load_pd( psw + vlen4 );
            rs05 = _t_load_pd( psw + vlen5 );
            
            rs04 = _t_fmadd_pd( rpj0, rccB, rs04 );
            rs05 = _t_fmadd_pd( rpj1, rccB, rs05 );
            
            _t_store_pd( psw + vlen4, rs04 );
            _t_store_pd( psw + vlen5, rs05 );
            
            rs06 = _t_load_pd( psw + vlen6 );
            rs07 = _t_load_pd( psw + vlen7 );
            
            rs06 = _t_fmadd_pd( rpj2, rccB, rs06 );
            rs07 = _t_fmadd_pd( rpj3, rccB, rs07 );
            
            _t_store_pd( psw + vlen6, rs06 );
            _t_store_pd( psw + vlen7, rs07 );
            
            #else
            
            rs00 = _t_load_pd( psw + vlen0 );
            rs01 = _t_load_pd( psw + vlen1 );
            
            rs04 = _t_mul_pd( rpj0, rccA );
            rs05 = _t_mul_pd( rpj1, rccA );
            
            rs02 = _t_load_pd( psw + vlen2 );
            rs03 = _t_load_pd( psw + vlen3 );
            
            rs06 = _t_mul_pd( rpj2, rccA );
            rs07 = _t_mul_pd( rpj3, rccA );
            
            rs00 = _t_add_pd( rs00, rs04 );
            rs01 = _t_add_pd( rs01, rs05 );
            
            _t_store_pd( psw + vlen0, rs00 );
            _t_store_pd( psw + vlen1, rs01 );
            
            rs02 = _t_add_pd( rs02, rs06 );
            rs03 = _t_add_pd( rs03, rs07 );
            
            _t_store_pd( psw + vlen2, rs02 );
            _t_store_pd( psw + vlen3, rs03 );
            
            rs00 = _t_load_pd( psw + vlen4 );
            rs01 = _t_load_pd( psw + vlen5 );
            
            rs04 = _t_mul_pd( rpj0, rccB );
            rs05 = _t_mul_pd( rpj1, rccB );
            
            rs02 = _t_load_pd( psw + vlen6 );
            rs03 = _t_load_pd( psw + vlen7 );
            
            rs06 = _t_mul_pd( rpj2, rccB );
            rs07 = _t_mul_pd( rpj3, rccB );
            
            rs00 = _t_add_pd( rs00, rs04 );
            rs01 = _t_add_pd( rs01, rs05 );
            
            _t_store_pd( psw + vlen4, rs00 );
            _t_store_pd( psw + vlen5, rs01 );
            
            rs02 = _t_add_pd( rs02, rs06 );
            rs03 = _t_add_pd( rs03, rs07 );
            
            _t_store_pd( psw + vlen6, rs02 );
            _t_store_pd( psw + vlen7, rs03 );
            
            #endif
            
            rccA = _t_set1_pd( *( pcc + 2 ) );
            rccB = _t_set1_pd( *( pcc + 3 ) );
            
            #if defined (__FMA__)
            
            rs00 = _t_load_pd( psw + vlen8 );
            rs01 = _t_load_pd( psw + vlen9 );
            
            rs00 = _t_fmadd_pd( rpj0, rccA, rs00 );
            rs01 = _t_fmadd_pd( rpj1, rccA, rs01 );
            
            _t_store_pd( psw + vlen8, rs00 );
            _t_store_pd( psw + vlen9, rs01 );
            
            rs02 = _t_load_pd( psw + vlen10 );
            rs03 = _t_load_pd( psw + vlen11 );
            
            rs02 = _t_fmadd_pd( rpj2, rccA, rs02 );
            rs03 = _t_fmadd_pd( rpj3, rccA, rs03 );
            
            _t_store_pd( psw + vlen10, rs02 );
            _t_store_pd( psw + vlen11, rs03 );
            
            rs04 = _t_load_pd( psw + vlen12 );
            rs05 = _t_load_pd( psw + vlen13 );
            
            rs04 = _t_fmadd_pd( rpj0, rccB, rs04 );
            rs05 = _t_fmadd_pd( rpj1, rccB, rs05 );
            
            _t_store_pd( psw + vlen12, rs04 );
            _t_store_pd( psw + vlen13, rs05 );
            
            rs06 = _t_load_pd( psw + vlen14 );
            rs07 = _t_load_pd( psw + vlen15 );
            
            rs06 = _t_fmadd_pd( rpj2, rccB, rs06 );
            rs07 = _t_fmadd_pd( rpj3, rccB, rs07 );
            
            _t_store_pd( psw + vlen14, rs06 );
            _t_store_pd( psw + vlen15, rs07 );
            
            #else
            
            rs00 = _t_load_pd( psw + vlen8 );
            rs01 = _t_load_pd( psw + vlen9 );
            
            rs04 = _t_mul_pd( rpj0, rccA );
            rs05 = _t_mul_pd( rpj1, rccA );
            
            rs02 = _t_load_pd( psw + vlen10 );
            rs03 = _t_load_pd( psw + vlen11 );
            
            rs06 = _t_mul_pd( rpj2, rccA );
            rs07 = _t_mul_pd( rpj3, rccA );
            
            rs00 = _t_add_pd( rs00, rs04 );
            rs01 = _t_add_pd( rs01, rs05 );
            
            _t_store_pd( psw + vlen8, rs00 );
            _t_store_pd( psw + vlen9, rs01 );
            
            rs02 = _t_add_pd( rs02, rs06 );
            rs03 = _t_add_pd( rs03, rs07 );
            
            _t_store_pd( psw + vlen10, rs02 );
            _t_store_pd( psw + vlen11, rs03 );
            
            rs00 = _t_load_pd( psw + vlen12 );
            rs01 = _t_load_pd( psw + vlen13 );
            
            rs04 = _t_mul_pd( rpj0, rccB );
            rs05 = _t_mul_pd( rpj1, rccB );
            
            rs02 = _t_load_pd( psw + vlen14 );
            rs03 = _t_load_pd( psw + vlen15 );
            
            rs06 = _t_mul_pd( rpj2, rccB );
            rs07 = _t_mul_pd( rpj3, rccB );
            
            rs00 = _t_add_pd( rs00, rs04 );
            rs01 = _t_add_pd( rs01, rs05 );
            
            _t_store_pd( psw + vlen12, rs00 );
            _t_store_pd( psw + vlen13, rs01 );
            
            rs02 = _t_add_pd( rs02, rs06 );
            rs03 = _t_add_pd( rs03, rs07 );
            
            _t_store_pd( psw + vlen14, rs02 );
            _t_store_pd( psw + vlen15, rs03 );
            
            #endif
            
            pcc += 4;
            psw += vlen16;
            
        }
        
    }
    
}