#include "pmj_set.h"

extern inline __attribute__((always_inline))
void bwd_set_c( const int n,
                const int ma1,
                const double *restrict cff,
                const double *restrict cosx,
                const double *restrict sinx,
                const double *restrict cc,
                      double *restrict pmm,
                      double *restrict pmj1,
                      double *restrict pmj,
                      double *restrict swork )

{   
    
    // Registers to be used
    __td r00, r01, r02, r03, r04, r05, r06, r07, r08;
    
    // Polynomials set-up
    pmj_set_c( ma1, cff, cosx, sinx, pmm, pmj1, pmj );
    
    // Load the polynomials and hope, that the compiler
    // will just rename the registers
    r00 = _t_load_pd( pmj + vlen0 );
    r01 = _t_load_pd( pmj + vlen1 );
    r02 = _t_load_pd( pmj + vlen2 );
    r03 = _t_load_pd( pmj + vlen3 );
    
    // Memory address of partial sums and coeffs
    const double *pcc = cc;
          double *psw = swork;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        r04 = _t_set1_pd( *( pcc + 0 ) );
        
        r05 = _t_mul_pd( r00, r04 );
        r06 = _t_mul_pd( r01, r04 );
        r07 = _t_mul_pd( r02, r04 );
        r08 = _t_mul_pd( r03, r04 );
        
        _t_store_pd( psw + vlen0, r05 );
        _t_store_pd( psw + vlen1, r06 );
        _t_store_pd( psw + vlen2, r07 );
        _t_store_pd( psw + vlen3, r08 );
        
        r04 = _t_set1_pd( *( pcc + 1 ) );
        
        r05 = _t_mul_pd( r00, r04 );
        r06 = _t_mul_pd( r01, r04 );
        r07 = _t_mul_pd( r02, r04 );
        r08 = _t_mul_pd( r03, r04 );
        
        _t_store_pd( psw + vlen4, r05 );
        _t_store_pd( psw + vlen5, r06 );
        _t_store_pd( psw + vlen6, r07 );
        _t_store_pd( psw + vlen7, r08 );
        
        r04 = _t_set1_pd( *( pcc + 2 ) );
        
        r05 = _t_mul_pd( r00, r04 );
        r06 = _t_mul_pd( r01, r04 );
        r07 = _t_mul_pd( r02, r04 );
        r08 = _t_mul_pd( r03, r04 );
        
        _t_store_pd( psw + vlen8,  r05 );
        _t_store_pd( psw + vlen9,  r06 );
        _t_store_pd( psw + vlen10, r07 );
        _t_store_pd( psw + vlen11, r08 );
        
        r04 = _t_set1_pd( *( pcc + 3 ) );
        
        r05 = _t_mul_pd( r00, r04 );
        r06 = _t_mul_pd( r01, r04 );
        r07 = _t_mul_pd( r02, r04 );
        r08 = _t_mul_pd( r03, r04 );
        
        _t_store_pd( psw + vlen12, r05 );
        _t_store_pd( psw + vlen13, r06 );
        _t_store_pd( psw + vlen14, r07 );
        _t_store_pd( psw + vlen15, r08 );
        
        pcc += 4;
        psw += 4 * vlen4;
        
    }
    
}