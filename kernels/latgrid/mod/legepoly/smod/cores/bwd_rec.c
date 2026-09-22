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
        r00 = _t_load_pd( pmj + vlen0 );
        r01 = _t_load_pd( pmj + vlen1 );
        r02 = _t_load_pd( pmj + vlen2 );
        r03 = _t_load_pd( pmj + vlen3 );
        
        // Loop over number of spectral rows
        for ( int i3 = 0; i3 < n; i3++ ) {
            
            r04 = _t_set1_pd( *( pcc + 0 ) );
            
            r05 = _t_load_pd( psw + vlen0 );
            r06 = _t_load_pd( psw + vlen1 );
            r07 = _t_load_pd( psw + vlen2 );
            r08 = _t_load_pd( psw + vlen3 );
            
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
            
            _t_store_pd( psw + vlen0, r05 );
            _t_store_pd( psw + vlen1, r06 );
            _t_store_pd( psw + vlen2, r07 );
            _t_store_pd( psw + vlen3, r08 );
            
            r04 = _t_set1_pd( *( pcc + 1 ) );
            
            r05 = _t_load_pd( psw + vlen4 );
            r06 = _t_load_pd( psw + vlen5 );
            r07 = _t_load_pd( psw + vlen6 );
            r08 = _t_load_pd( psw + vlen7 );
            
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
            
            _t_store_pd( psw + vlen4, r05 );
            _t_store_pd( psw + vlen5, r06 );
            _t_store_pd( psw + vlen6, r07 );
            _t_store_pd( psw + vlen7, r08 );
            
            r04 = _t_set1_pd( *( pcc + 2 ) );
            
            r05 = _t_load_pd( psw + vlen8  );
            r06 = _t_load_pd( psw + vlen9  );
            r07 = _t_load_pd( psw + vlen10 );
            r08 = _t_load_pd( psw + vlen11 );
            
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
            
            _t_store_pd( psw + vlen8 , r05 );
            _t_store_pd( psw + vlen9 , r06 );
            _t_store_pd( psw + vlen10, r07 );
            _t_store_pd( psw + vlen11, r08 );
            
            r04 = _t_set1_pd( *( pcc + 3 ) );
            
            r05 = _t_load_pd( psw + vlen12 );
            r06 = _t_load_pd( psw + vlen13 );
            r07 = _t_load_pd( psw + vlen14 );
            r08 = _t_load_pd( psw + vlen15 );
            
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
            
            _t_store_pd( psw + vlen12, r05 );
            _t_store_pd( psw + vlen13, r06 );
            _t_store_pd( psw + vlen14, r07 );
            _t_store_pd( psw + vlen15, r08 );
            
            pcc += 4;
            psw += 4 * vlen4;
            
        }
        
    }
    
}