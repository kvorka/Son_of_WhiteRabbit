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
            
            r05 = _t_load_pd( sinx + vlen0 );
            r06 = _t_load_pd( sinx + vlen1 );
            r07 = _t_load_pd( sinx + vlen2 );
            r08 = _t_load_pd( sinx + vlen3 );
            
            r05 = _t_mul_pd( r04, r05 );
            r06 = _t_mul_pd( r04, r06 );
            r07 = _t_mul_pd( r04, r07 );
            r08 = _t_mul_pd( r04, r08 );
            
            r00 = _t_load_pd( pmm + vlen0 );
            r01 = _t_load_pd( pmm + vlen1 );
            r02 = _t_load_pd( pmm + vlen2 );
            r03 = _t_load_pd( pmm + vlen3 );
            
            r00 = _t_mul_pd( r05, r00 );
            r01 = _t_mul_pd( r06, r01 );
            r02 = _t_mul_pd( r07, r02 );
            r03 = _t_mul_pd( r08, r03 );
            
        }
        
        _t_store_pd( pmm + vlen0, r00 );
        _t_store_pd( pmm + vlen1, r01 );
        _t_store_pd( pmm + vlen2, r02 );
        _t_store_pd( pmm + vlen3, r03 );
        
    }
    
    // pmj1 and pmj set-up
    {
        
        r08 = _t_setzero_pd();
        
        _t_store_pd( pmj1 + vlen0, r08 );
        _t_store_pd( pmj1 + vlen1, r08 );
        _t_store_pd( pmj1 + vlen2, r08 );
        _t_store_pd( pmj1 + vlen3, r08 );
        
        r04 = _t_load_pd( cosx + vlen0 );
        r05 = _t_load_pd( cosx + vlen1 );
        r06 = _t_load_pd( cosx + vlen2 );
        r07 = _t_load_pd( cosx + vlen3 );
        
        r00 = _t_div_pd( r00, r04 );
        r01 = _t_div_pd( r01, r05 );
        r02 = _t_div_pd( r02, r06 );
        r03 = _t_div_pd( r03, r07 );
        
        _t_store_pd( pmj + vlen0, r00 );
        _t_store_pd( pmj + vlen1, r01 );
        _t_store_pd( pmj + vlen2, r02 );
        _t_store_pd( pmj + vlen3, r03 );
        
    }
    
}