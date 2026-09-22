#pragma once
#include "../../../../../math/cvec.h"

static inline __attribute__((always_inline))
void pmj_rec_c( const double *restrict fmj,
                const double *restrict cosx2,
                      double *restrict pmj1,
                      double *restrict pmj )

{
    
    // Registers to be used
    __td r00, r01, r02, r03,
         r04, r05, r06, r07,
         r08, r09, r10, r11;
    
    // Body of the reccurence
    r04 = _t_set1_pd( *( fmj + 0 ) );
    r05 = _t_set1_pd( *( fmj + 1 ) );
    r08 = _t_set1_pd( *( fmj + 2 ) );
    
    r00 = _t_load_pd( cosx2 + vlen0 );
    r01 = _t_load_pd( cosx2 + vlen1 );
    r02 = _t_load_pd( cosx2 + vlen2 );
    r03 = _t_load_pd( cosx2 + vlen3 );
    
    #if defined (__FMA__)
    r00 = _t_fmsub_pd( r04, r00, r05 );
    r01 = _t_fmsub_pd( r04, r01, r05 );
    r02 = _t_fmsub_pd( r04, r02, r05 );
    r03 = _t_fmsub_pd( r04, r03, r05 );
    #else
    r00 = _t_mul_pd( r04, r00 );
    r01 = _t_mul_pd( r04, r01 );
    r02 = _t_mul_pd( r04, r02 );
    r03 = _t_mul_pd( r04, r03 );
    
    r00 = _t_sub_pd( r00, r05 );
    r01 = _t_sub_pd( r01, r05 );
    r02 = _t_sub_pd( r02, r05 );
    r03 = _t_sub_pd( r03, r05 );
    #endif
    
    r04 = _t_load_pd( pmj1 + vlen0 );
    r05 = _t_load_pd( pmj1 + vlen1 );
    r06 = _t_load_pd( pmj1 + vlen2 );
    r07 = _t_load_pd( pmj1 + vlen3 );
    
    r04 = _t_mul_pd( r08, r04 );
    r05 = _t_mul_pd( r08, r05 );
    r06 = _t_mul_pd( r08, r06 );
    r07 = _t_mul_pd( r08, r07 );
    
    r08 = _t_load_pd( pmj + vlen0 );
    r09 = _t_load_pd( pmj + vlen1 );
    r10 = _t_load_pd( pmj + vlen2 );
    r11 = _t_load_pd( pmj + vlen3 );
    
    _t_store_pd( pmj1 + vlen0, r08 );
    _t_store_pd( pmj1 + vlen1, r09 );
    _t_store_pd( pmj1 + vlen2, r10 );
    _t_store_pd( pmj1 + vlen3, r11 );
    
    #if defined (__FMA__)
    r00 = _t_fmsub_pd( r00, r08, r04 );
    r01 = _t_fmsub_pd( r01, r09, r05 );
    r02 = _t_fmsub_pd( r02, r10, r06 );
    r03 = _t_fmsub_pd( r03, r11, r07 );
    #else
    r00 = _t_mul_pd( r00, r08);
    r01 = _t_mul_pd( r01, r09);
    r02 = _t_mul_pd( r02, r10);
    r03 = _t_mul_pd( r03, r11);
    
    r00 = _t_sub_pd( r00, r04 );
    r01 = _t_sub_pd( r01, r05 );
    r02 = _t_sub_pd( r02, r06 );
    r03 = _t_sub_pd( r03, r07 );
    #endif
    
    _t_store_pd( pmj + vlen0, r00 );
    _t_store_pd( pmj + vlen1, r01 );
    _t_store_pd( pmj + vlen2, r02 );
    _t_store_pd( pmj + vlen3, r03 );
    
}