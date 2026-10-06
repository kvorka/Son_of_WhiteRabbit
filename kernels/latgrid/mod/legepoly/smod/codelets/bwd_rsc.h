#pragma once
#include "../../../../../math/cvec.h"

static inline __attribute__((always_inline))
void bwd_rsc_c( const int n,
                const double *restrict cosx,
                const double *restrict swork,
                      double *restrict sumN,
                      double *restrict sumS )

{
    
    // Memory walking constant
    const ptrdiff_t stepNS = n * vlen4;
    
    // Memory pointers
    const double *restrict psworkA = swork;
    const double *restrict psworkB = swork + vlen8 * n;
          double *restrict psumN;
          double *restrict psumS;
    
    // Cosine registers
    const __td rcx0 = _t_load_pd( cosx + vlen0 );
    const __td rcx1 = _t_load_pd( cosx + vlen1 );
    const __td rcx2 = _t_load_pd( cosx + vlen2 );
    const __td rcx3 = _t_load_pd( cosx + vlen3 );
    
    // Other registers to be used
    __td rsA0, rsA1, rsB0, rsB1, rN0, rN1, rS0, rS1;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + vlen4 * i3;
        psumS = sumS + vlen4 * i3;
        
        #if defined (__FMA__)
        
        rsA0 = _t_load_pd( psworkA + vlen0 );
        rsB0 = _t_load_pd( psworkB + vlen0 );
        
        rN0 = _t_fmadd_pd( rsB0, rcx0, rsA0 );
        rS0 = _t_fmsub_pd( rsB0, rcx0, rsA0 );
        
        _t_store_pd( psumN + vlen0, rN0 );
        _t_store_pd( psumS + vlen0, rS0 );
        
        rsA1 = _t_load_pd( psworkA + vlen1 );
        rsB1 = _t_load_pd( psworkB + vlen1 );
        
        rN1 = _t_fmadd_pd( rsB1, rcx1, rsA1 );
        rS1 = _t_fmsub_pd( rsB1, rcx1, rsA1 );
        
        _t_store_pd( psumN + vlen1, rN1 );
        _t_store_pd( psumS + vlen1, rS1 );
        
        rsA0 = _t_load_pd( psworkA + vlen2 );
        rsB0 = _t_load_pd( psworkB + vlen2 );
        
        rN0 = _t_fmadd_pd( rsB0, rcx2, rsA0 );
        rS0 = _t_fmsub_pd( rsB0, rcx2, rsA0 );
        
        _t_store_pd( psumN + vlen2, rN0 );
        _t_store_pd( psumS + vlen2, rS0 );
        
        rsA1 = _t_load_pd( psworkA + vlen3 );
        rsB1 = _t_load_pd( psworkB + vlen3 );
        
        rN1 = _t_fmadd_pd( rsB1, rcx3, rsA1 );
        rS1 = _t_fmsub_pd( rsB1, rcx3, rsA1 );
        
        _t_store_pd( psumN + vlen3, rN1 );
        _t_store_pd( psumS + vlen3, rS1 );
        
        psumN += stepNS;
        psumS += stepNS;
        
        rsA0 = _t_load_pd( psworkA + vlen4 );
        rsB0 = _t_load_pd( psworkB + vlen4 );
        
        rN0 = _t_fmadd_pd( rsB0, rcx0, rsA0 );
        rS0 = _t_fmsub_pd( rsB0, rcx0, rsA0 );
        
        _t_store_pd( psumN + vlen0, rN0 );
        _t_store_pd( psumS + vlen0, rS0 );
        
        rsA1 = _t_load_pd( psworkA + vlen5 );
        rsB1 = _t_load_pd( psworkB + vlen5 );
        
        rN1 = _t_fmadd_pd( rsB1, rcx1, rsA1 );
        rS1 = _t_fmsub_pd( rsB1, rcx1, rsA1 );
        
        _t_store_pd( psumN + vlen1, rN1 );
        _t_store_pd( psumS + vlen1, rS1 );
        
        rsA0 = _t_load_pd( psworkA + vlen6 );
        rsB0 = _t_load_pd( psworkB + vlen6 );
        
        rN0 = _t_fmadd_pd( rsB0, rcx2, rsA0 );
        rS0 = _t_fmsub_pd( rsB0, rcx2, rsA0 );
        
        _t_store_pd( psumN + vlen2, rN0 );
        _t_store_pd( psumS + vlen2, rS0 );
        
        rsA1 = _t_load_pd( psworkA + vlen7 );
        rsB1 = _t_load_pd( psworkB + vlen7 );
        
        rN1 = _t_fmadd_pd( rsB1, rcx3, rsA1 );
        rS1 = _t_fmsub_pd( rsB1, rcx3, rsA1 );
        
        _t_store_pd( psumN + vlen3, rN1 );
        _t_store_pd( psumS + vlen3, rS1 );
        
        psworkA += vlen8;
        psworkB += vlen8;
        
        #else
        
        rsB0 = _t_load_pd( psworkB + vlen0 );
        rsB1 = _t_load_pd( psworkB + vlen1 );
        
        rsB0 = _t_mul_pd( rsB0, rcx0 );
        rsB1 = _t_mul_pd( rsB1, rcx1 );
        
        rsA0 = _t_load_pd( psworkA + vlen0 );
        rsA1 = _t_load_pd( psworkA + vlen1 );
        
        rN0 = _t_add_pd( rsB0, rsA0 );
        rN1 = _t_add_pd( rsB1, rsA1 );
        
        _t_store_pd( psumN + vlen0, rN0 );
        _t_store_pd( psumN + vlen1, rN1 );
        
        rS0 = _t_sub_pd( rsB0, rsA0 );
        rS1 = _t_sub_pd( rsB1, rsA1 );
        
        _t_store_pd( psumS + vlen0, rS0 );
        _t_store_pd( psumS + vlen1, rS1 );
        
        rsB0 = _t_load_pd( psworkB + vlen2 );
        rsB1 = _t_load_pd( psworkB + vlen3 );
        
        rsB0 = _t_mul_pd( rsB0, rcx2 );
        rsB1 = _t_mul_pd( rsB1, rcx3 );
        
        rsA0 = _t_load_pd( psworkA + vlen2 );
        rsA1 = _t_load_pd( psworkA + vlen3 );
        
        rN0 = _t_add_pd( rsB0, rsA0 );
        rN1 = _t_add_pd( rsB1, rsA1 );
        
        _t_store_pd( psumN + vlen2, rN0 );
        _t_store_pd( psumN + vlen3, rN1 );
        
        rS0 = _t_sub_pd( rsB0, rsA0 );
        rS1 = _t_sub_pd( rsB1, rsA1 );
        
        _t_store_pd( psumS + vlen2, rS0 );
        _t_store_pd( psumS + vlen3, rS1 );
        
        psumN += stepNS;
        psumS += stepNS;
        
        rsB0 = _t_load_pd( psworkB + vlen4 );
        rsB1 = _t_load_pd( psworkB + vlen5 );
        
        rsB0 = _t_mul_pd( rsB0, rcx0 );
        rsB1 = _t_mul_pd( rsB1, rcx1 );
        
        rsA0 = _t_load_pd( psworkA + vlen4 );
        rsA1 = _t_load_pd( psworkA + vlen5 );
        
        rN0 = _t_add_pd( rsB0, rsA0 );
        rN1 = _t_add_pd( rsB1, rsA1 );
        
        _t_store_pd( psumN + vlen0, rN0 );
        _t_store_pd( psumN + vlen1, rN1 );
        
        rS0 = _t_sub_pd( rsB0, rsA0 );
        rS1 = _t_sub_pd( rsB1, rsA1 );
        
        _t_store_pd( psumS + vlen0, rS0 );
        _t_store_pd( psumS + vlen1, rS1 );
        
        rsB0 = _t_load_pd( psworkB + vlen6 );
        rsB1 = _t_load_pd( psworkB + vlen7 );
        
        rsB0 = _t_mul_pd( rsB0, rcx2 );
        rsB1 = _t_mul_pd( rsB1, rcx3 );
        
        rsA0 = _t_load_pd( psworkA + vlen6 );
        rsA1 = _t_load_pd( psworkA + vlen7 );
        
        rN0 = _t_add_pd( rsB0, rsA0 );
        rN1 = _t_add_pd( rsB1, rsA1 );
        
        _t_store_pd( psumN + vlen2, rN0 );
        _t_store_pd( psumN + vlen3, rN1 );
        
        rS0 = _t_sub_pd( rsB0, rsA0 );
        rS1 = _t_sub_pd( rsB1, rsA1 );
        
        _t_store_pd( psumS + vlen2, rS0 );
        _t_store_pd( psumS + vlen3, rS1 );
        
        psworkA += vlen8;
        psworkB += vlen8;
        
        #endif
        
    }
    
}