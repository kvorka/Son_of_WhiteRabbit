#pragma once
#include "../../../../../math/cvec.h"

static inline __attribute__((always_inline))
void pmj_set_c( const int ma1,
                const double *restrict cff,
                const double *restrict cosx,
                const double *restrict sinx,
                      double *restrict pmm,
                      double *restrict pmj1,
                      double *restrict pmj )

{
    
    // Registers to be used
    __td r00, r01, r02, r03, r04, r05, r06, r07, r08;
    
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
    
}