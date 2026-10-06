#pragma once
#include "../../../../../math/cvec.h"

static inline __attribute__((always_inline))
void pmj_set_c( const int m,
                const double *restrict cff,
                const double *restrict cosx,
                const double *restrict sinx,
                      double *restrict pmm,
                      double *restrict pmj1,
                      double *restrict pmj )

{
    
    // Constant registers
    const __td rff = _t_set1_pd( *cff );
    const __td r00 = _t_setzero_pd();
    
    // Registers to be used
    __td rmm0, rmm1, rmm2, rmm3,
         rsx0, rsx1, rsx2, rsx3,
         rcx0, rcx1, rcx2, rcx3;
    
    // Main code
    if ( m == 0 ) {
        
        _t_store_pd( pmj1 + vlen0, r00 );
        _t_store_pd( pmj1 + vlen1, r00 );
        
        rcx0 = _t_load_pd( cosx + vlen0 );
        rcx1 = _t_load_pd( cosx + vlen1 );
        
        _t_store_pd( pmj1 + vlen0, r00 );
        _t_store_pd( pmj1 + vlen1, r00 );
        
        rcx2 = _t_load_pd( cosx + vlen2 );
        rcx3 = _t_load_pd( cosx + vlen3 );
        
        _t_store_pd( pmm + vlen0, rff );
        _t_store_pd( pmm + vlen1, rff );
        
        rcx0 = _t_div_pd( rff, rcx0 );
        rcx1 = _t_div_pd( rff, rcx1 );
        
        _t_store_pd( pmm + vlen2, rff  );
        _t_store_pd( pmm + vlen3, rff  );
        _t_store_pd( pmj + vlen0, rcx0 );
        _t_store_pd( pmj + vlen1, rcx1 );
        
        rcx2 = _t_div_pd( rff, rcx2 );
        rcx3 = _t_div_pd( rff, rcx3 );
        
        _t_store_pd( pmj + vlen2, rcx2 );
        _t_store_pd( pmj + vlen3, rcx3 );
        
    } else {
        
        rsx0 = _t_load_pd( sinx + vlen0 );
        rsx1 = _t_load_pd( sinx + vlen1 );
        
        rsx0 = _t_mul_pd( rff, rsx0 );
        rsx1 = _t_mul_pd( rff, rsx1 );
        
        _t_store_pd( pmj1 + vlen0, r00 );
        _t_store_pd( pmj1 + vlen1, r00 );
        
        rsx2 = _t_load_pd( sinx + vlen2 );
        rsx3 = _t_load_pd( sinx + vlen3 );
        
        rsx2 = _t_mul_pd( rff, rsx2 );
        rsx3 = _t_mul_pd( rff, rsx3 );
        
        _t_store_pd( pmj1 + vlen2, r00 );
        _t_store_pd( pmj1 + vlen3, r00 );
        
        rmm0 = _t_load_pd( pmm + vlen0 );
        rmm1 = _t_load_pd( pmm + vlen1 );
        
        rmm0 = _t_mul_pd( rsx0, rmm0 );
        rmm1 = _t_mul_pd( rsx1, rmm1 );
        
        _t_store_pd( pmm + vlen0, rmm0 );
        _t_store_pd( pmm + vlen1, rmm1 );
        
        rmm2 = _t_load_pd( pmm + vlen2 );
        rmm3 = _t_load_pd( pmm + vlen3 );
        
        rmm2 = _t_mul_pd( rsx2, rmm2 );
        rmm3 = _t_mul_pd( rsx3, rmm3 );
        
        _t_store_pd( pmm + vlen2, rmm2 );
        _t_store_pd( pmm + vlen3, rmm3 );
        
        rcx0 = _t_load_pd( cosx + vlen0 );
        rcx1 = _t_load_pd( cosx + vlen1 );
        
        rcx0 = _t_div_pd( rmm0, rcx0 );
        rcx1 = _t_div_pd( rmm1, rcx1 );
        
        _t_store_pd( pmj + vlen0, rcx0 );
        _t_store_pd( pmj + vlen1, rcx1 );
        
        rcx2 = _t_load_pd( cosx + vlen2 );
        rcx3 = _t_load_pd( cosx + vlen3 );
        
        rcx2 = _t_div_pd( rmm2, rcx2 );
        rcx3 = _t_div_pd( rmm3, rcx3 );
        
        _t_store_pd( pmj + vlen2, rcx2 );
        _t_store_pd( pmj + vlen3, rcx3 );
        
    }
    
}