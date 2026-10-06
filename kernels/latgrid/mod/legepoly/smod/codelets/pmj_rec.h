#pragma once
#include "../../../../../math/cvec.h"

static inline __attribute__((always_inline))
void pmj_rec_c( const double *restrict fmj,
                const double *restrict cosx2,
                      double *restrict pmj1,
                      double *restrict pmj )

{
    
    // Registers to be used
    __td rrc0, rrc1, rrc2, rrc3,
         rp10, rp11, rp12, rp13,
         rpj0, rpj1, rpj2, rpj3;
    
    // First FMA coefficient and pmj1 * cff3
    {
        
        const __td rff1 = _t_set1_pd( *( fmj + 0 ) );
        const __td rff2 = _t_set1_pd( *( fmj + 1 ) );
        const __td rff3 = _t_set1_pd( *( fmj + 2 ) );
        
        #if defined (__FMA__)
        
        rrc0 = _t_load_pd( cosx2 + vlen0 );
        rrc1 = _t_load_pd( cosx2 + vlen1 );
        
        rrc0 = _t_fmsub_pd( rff1, rrc0, rff2 );
        rrc1 = _t_fmsub_pd( rff1, rrc1, rff2 );
        
        rrc2 = _t_load_pd( cosx2 + vlen2 );
        rrc3 = _t_load_pd( cosx2 + vlen3 );
        
        rrc2 = _t_fmsub_pd( rff1, rrc2, rff2 );
        rrc3 = _t_fmsub_pd( rff1, rrc3, rff2 );
        
        rp10 = _t_load_pd( pmj1 + vlen0 );
        rp11 = _t_load_pd( pmj1 + vlen1 );
        
        rp10 = _t_mul_pd( rff3, rp10 );
        rp11 = _t_mul_pd( rff3, rp11 );
        
        rp12 = _t_load_pd( pmj1 + vlen2 );
        rp13 = _t_load_pd( pmj1 + vlen3 );
        
        rp12 = _t_mul_pd( rff3, rp12 );
        rp13 = _t_mul_pd( rff3, rp13 );
        
        #else
        
        rrc0 = _t_load_pd( cosx2 + vlen0 );
        rrc1 = _t_load_pd( cosx2 + vlen1 );
        
        rrc0 = _t_mul_pd( rff1, rrc0 );
        rrc1 = _t_mul_pd( rff1, rrc1 );
        
        rrc2 = _t_load_pd( cosx2 + vlen2 );
        rrc3 = _t_load_pd( cosx2 + vlen3 );
        
        rrc0 = _t_sub_pd( rrc0, rff2 );
        rrc1 = _t_sub_pd( rrc1, rff2 );
        
        rp10 = _t_load_pd( pmj1 + vlen0 );
        rp11 = _t_load_pd( pmj1 + vlen1 );
        
        rrc2 = _t_mul_pd( rff1, rrc2 );
        rrc3 = _t_mul_pd( rff1, rrc3 );
        rp10 = _t_mul_pd( rff3, rp10 );
        rp11 = _t_mul_pd( rff3, rp11 );
        
        rp12 = _t_load_pd( pmj1 + vlen2 );
        rp13 = _t_load_pd( pmj1 + vlen3 );
        
        rrc2 = _t_sub_pd( rrc2, rff2 );
        rrc3 = _t_sub_pd( rrc3, rff2 );
        rp12 = _t_mul_pd( rff3, rp12 );
        rp13 = _t_mul_pd( rff3, rp13 );
        
        #endif
        
    }
    
    // Pmj1 -> Pmj and second FMA
    {
        
        #if defined (__FMA__)
        
        rpj0 = _t_load_pd( pmj + vlen0 );
        rpj1 = _t_load_pd( pmj + vlen1 );
        
        _t_store_pd( pmj1 + vlen0, rpj0 );
        _t_store_pd( pmj1 + vlen1, rpj1 );
        
        rpj2 = _t_load_pd( pmj + vlen2 );
        rpj3 = _t_load_pd( pmj + vlen3 );
        
        _t_store_pd( pmj1 + vlen2, rpj2 );
        _t_store_pd( pmj1 + vlen3, rpj3 );
        
        rrc0 = _t_fmsub_pd( rrc0, rpj0, rp10 );
        rrc1 = _t_fmsub_pd( rrc1, rpj1, rp11 );
        
        _t_store_pd( pmj  + vlen0, rrc0 );
        _t_store_pd( pmj  + vlen1, rrc1 );
        
        rrc2 = _t_fmsub_pd( rrc2, rpj2, rp12 );
        rrc3 = _t_fmsub_pd( rrc3, rpj3, rp13 );
        
        _t_store_pd( pmj + vlen2, rrc2 );
        _t_store_pd( pmj + vlen3, rrc3 );
        
        #else
        
        rpj0 = _t_load_pd( pmj + vlen0 );
        rpj1 = _t_load_pd( pmj + vlen1 );
        
        rrc0 = _t_mul_pd( rrc0, rpj0);
        rrc1 = _t_mul_pd( rrc1, rpj1);
        
        _t_store_pd( pmj1 + vlen0, rpj0 );
        _t_store_pd( pmj1 + vlen1, rpj1 );
        
        rpj2 = _t_load_pd( pmj + vlen2 );
        rpj3 = _t_load_pd( pmj + vlen3 );
        
        rrc2 = _t_mul_pd( rrc2, rpj2);
        rrc3 = _t_mul_pd( rrc3, rpj3);
        
        _t_store_pd( pmj1 + vlen2, rpj2 );
        _t_store_pd( pmj1 + vlen3, rpj3 );
        
        rrc0 = _t_sub_pd( rrc0, rp10 );
        rrc1 = _t_sub_pd( rrc1, rp11 );
        
        _t_store_pd( pmj + vlen0, rrc0 );
        _t_store_pd( pmj + vlen1, rrc1 );
        
        rrc2 = _t_sub_pd( rrc2, rp12 );
        rrc3 = _t_sub_pd( rrc3, rp13 );
        
        _t_store_pd( pmj + vlen2, rrc2 );
        _t_store_pd( pmj + vlen3, rrc3 );
        
        #endif
        
    }
    
}