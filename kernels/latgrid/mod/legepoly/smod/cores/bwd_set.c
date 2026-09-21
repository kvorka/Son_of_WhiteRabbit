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
    r00 = _t_load_pd( pmj + 0*vlen );
    r01 = _t_load_pd( pmj + 1*vlen );
    r02 = _t_load_pd( pmj + 2*vlen );
    r03 = _t_load_pd( pmj + 3*vlen );
    
    // Memory address of partial sums and coeffs
    const double *pcc = cc;
          double *psw = swork;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        r04 = _t_set1_pd( *( pcc + 0 ) );
        
        r05 = _t_mul_pd( r00, r04 );
        r06 = _t_mul_pd( r01, r04 );
        r07 = _t_mul_pd( r02, r04 );
        r08 = _t_mul_pd( r03, r04 );
        
        _t_store_pd( psw + 0*vlen, r05 );
        _t_store_pd( psw + 1*vlen, r06 );
        _t_store_pd( psw + 2*vlen, r07 );
        _t_store_pd( psw + 3*vlen, r08 );
        
        r04 = _t_set1_pd( *( pcc + 1 ) );
        
        r05 = _t_mul_pd( r00, r04 );
        r06 = _t_mul_pd( r01, r04 );
        r07 = _t_mul_pd( r02, r04 );
        r08 = _t_mul_pd( r03, r04 );
        
        _t_store_pd( psw + 4*vlen, r05 );
        _t_store_pd( psw + 5*vlen, r06 );
        _t_store_pd( psw + 6*vlen, r07 );
        _t_store_pd( psw + 7*vlen, r08 );
        
        r04 = _t_set1_pd( *( pcc + 2 ) );
        
        r05 = _t_mul_pd( r00, r04 );
        r06 = _t_mul_pd( r01, r04 );
        r07 = _t_mul_pd( r02, r04 );
        r08 = _t_mul_pd( r03, r04 );
        
        _t_store_pd( psw +  8*vlen, r05 );
        _t_store_pd( psw +  9*vlen, r06 );
        _t_store_pd( psw + 10*vlen, r07 );
        _t_store_pd( psw + 11*vlen, r08 );
        
        r04 = _t_set1_pd( *( pcc + 3 ) );
        
        r05 = _t_mul_pd( r00, r04 );
        r06 = _t_mul_pd( r01, r04 );
        r07 = _t_mul_pd( r02, r04 );
        r08 = _t_mul_pd( r03, r04 );
        
        _t_store_pd( psw + 12*vlen, r05 );
        _t_store_pd( psw + 13*vlen, r06 );
        _t_store_pd( psw + 14*vlen, r07 );
        _t_store_pd( psw + 15*vlen, r08 );
        
        pcc +=  4;
        psw += 16 * vlen;
        
    }
    
}