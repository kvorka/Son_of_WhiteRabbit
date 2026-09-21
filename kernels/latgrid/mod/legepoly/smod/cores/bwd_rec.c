#include "pmj_rec.h"

extern inline __attribute__((always_inline))
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
    
    // Registers to be used, r00-r03 are reserved
    // for pmj values obtained from reccurrence
    __td r00, r01, r02, r03, r04, r05, r06, r07,
         r08, r09, r10, r11, r12;
    
    // Cycle over weird degree iterator
    for ( int i4 = 0; i4 < nma; i4++ ) {
        
        // Reset the accumulator
        psw = swork;
        
        // Legendre polynomial reccurence
        pmj_rec_c( fmj+3*i4, cosx2, pmj1, pmj );
        
        // Load the polynomials and hope, that the compiler
        // will just rename the registers
        r00 = _t_load_pd( pmj + 0*vlen );
        r01 = _t_load_pd( pmj + 1*vlen );
        r02 = _t_load_pd( pmj + 2*vlen );
        r03 = _t_load_pd( pmj + 3*vlen );
        
        // Loop over number of spectral rows
        for ( int i3 = 0; i3 < n; i3++ ) {
            
            r04 = _t_set1_pd( *( pcc + 0 ) );
            
            r05 = _t_load_pd( psw + 0*vlen );
            r06 = _t_load_pd( psw + 1*vlen );
            r07 = _t_load_pd( psw + 2*vlen );
            r08 = _t_load_pd( psw + 3*vlen );
            
            #if defined (__FMA__)
            r05 = _t_fmadd_pd( r00, r04, r05 );
            r06 = _t_fmadd_pd( r01, r04, r06 );
            r07 = _t_fmadd_pd( r02, r04, r07 );
            r08 = _t_fmadd_pd( r03, r04, r08 );
            #else
            r09 = _t_mul_pd( r00, r04 );
            r10 = _t_mul_pd( r01, r04 );
            r11 = _t_mul_pd( r02, r04 );
            r12 = _t_mul_pd( r03, r04 );
            
            r05 = _t_add_pd( r09, r05 );
            r06 = _t_add_pd( r10, r06 );
            r07 = _t_add_pd( r11, r07 );
            r08 = _t_add_pd( r12, r08 );
            #endif
            
            _t_store_pd( psw + 0*vlen, r05 );
            _t_store_pd( psw + 1*vlen, r06 );
            _t_store_pd( psw + 2*vlen, r07 );
            _t_store_pd( psw + 3*vlen, r08 );
            
            r04 = _t_set1_pd( *( pcc + 1 ) );
            
            r05 = _t_load_pd( psw + 4*vlen );
            r06 = _t_load_pd( psw + 5*vlen );
            r07 = _t_load_pd( psw + 6*vlen );
            r08 = _t_load_pd( psw + 7*vlen );
            
            #if defined (__FMA__)
            r05 = _t_fmadd_pd( r00, r04, r05 );
            r06 = _t_fmadd_pd( r01, r04, r06 );
            r07 = _t_fmadd_pd( r02, r04, r07 );
            r08 = _t_fmadd_pd( r03, r04, r08 );
            #else
            r09 = _t_mul_pd( r00, r04 );
            r10 = _t_mul_pd( r01, r04 );
            r11 = _t_mul_pd( r02, r04 );
            r12 = _t_mul_pd( r03, r04 );
            
            r05 = _t_add_pd( r09, r05 );
            r06 = _t_add_pd( r10, r06 );
            r07 = _t_add_pd( r11, r07 );
            r08 = _t_add_pd( r12, r08 );
            #endif
            
            _t_store_pd( psw + 4*vlen, r05 );
            _t_store_pd( psw + 5*vlen, r06 );
            _t_store_pd( psw + 6*vlen, r07 );
            _t_store_pd( psw + 7*vlen, r08 );
            
            r04 = _t_set1_pd( *( pcc + 2 ) );
            
            r05 = _t_load_pd( psw +  8*vlen );
            r06 = _t_load_pd( psw +  9*vlen );
            r07 = _t_load_pd( psw + 10*vlen );
            r08 = _t_load_pd( psw + 11*vlen );
            
            #if defined (__FMA__)
            r05 = _t_fmadd_pd( r00, r04, r05 );
            r06 = _t_fmadd_pd( r01, r04, r06 );
            r07 = _t_fmadd_pd( r02, r04, r07 );
            r08 = _t_fmadd_pd( r03, r04, r08 );
            #else
            r09 = _t_mul_pd( r00, r04 );
            r10 = _t_mul_pd( r01, r04 );
            r11 = _t_mul_pd( r02, r04 );
            r12 = _t_mul_pd( r03, r04 );
            
            r05 = _t_add_pd( r09, r05 );
            r06 = _t_add_pd( r10, r06 );
            r07 = _t_add_pd( r11, r07 );
            r08 = _t_add_pd( r12, r08 );
            #endif
            
            _t_store_pd( psw +  8*vlen, r05 );
            _t_store_pd( psw +  9*vlen, r06 );
            _t_store_pd( psw + 10*vlen, r07 );
            _t_store_pd( psw + 11*vlen, r08 );
            
            r04 = _t_set1_pd( *( pcc + 3 ) );
            
            r05 = _t_load_pd( psw + 12*vlen );
            r06 = _t_load_pd( psw + 13*vlen );
            r07 = _t_load_pd( psw + 14*vlen );
            r08 = _t_load_pd( psw + 15*vlen );
            
            #if defined (__FMA__)
            r05 = _t_fmadd_pd( r00, r04, r05 );
            r06 = _t_fmadd_pd( r01, r04, r06 );
            r07 = _t_fmadd_pd( r02, r04, r07 );
            r08 = _t_fmadd_pd( r03, r04, r08 );
            #else
            r09 = _t_mul_pd( r00, r04 );
            r10 = _t_mul_pd( r01, r04 );
            r11 = _t_mul_pd( r02, r04 );
            r12 = _t_mul_pd( r03, r04 );
            
            r05 = _t_add_pd( r09, r05 );
            r06 = _t_add_pd( r10, r06 );
            r07 = _t_add_pd( r11, r07 );
            r08 = _t_add_pd( r12, r08 );
            #endif
            
            _t_store_pd( psw + 12*vlen, r05 );
            _t_store_pd( psw + 13*vlen, r06 );
            _t_store_pd( psw + 14*vlen, r07 );
            _t_store_pd( psw + 15*vlen, r08 );
            
            pcc +=  4;
            psw += 16 * vlen;
            
        }
        
    }
    
}