#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void bwd_rsc_c( const int n,
                const double *restrict cosx,
                const double *restrict swork,
                      double *restrict sumN,
                      double *restrict sumS )

{
    
    // memory pointers
    const double *restrict pswork1 = swork;
    const double *restrict pswork2 = swork + 2 * vlen4 * n;
          double *restrict psumN;
          double *restrict psumS;
    
    // cosine registers
    const __td rcosx0 = _t_load_pd( cosx + vlen0 );
    const __td rcosx1 = _t_load_pd( cosx + vlen1 );
    const __td rcosx2 = _t_load_pd( cosx + vlen2 );
    const __td rcosx3 = _t_load_pd( cosx + vlen3 );
    
    // other registers to be used
    __td r00, r01, r02, r03, r04, r05, r06, r07;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + vlen4 * i3;
        psumS = sumS + vlen4 * i3;
        
        for ( int i2 = 0; i2 < 2; i2++ ) {
            
            r00 = _t_load_pd( pswork1 + vlen0 );
            r01 = _t_load_pd( pswork1 + vlen1 );
            r02 = _t_load_pd( pswork2 + vlen0 );
            r03 = _t_load_pd( pswork2 + vlen1 );
            
            #if defined (__FMA__)
            r04 = _t_fmadd_pd( r02, rcosx0, r00 );
            r05 = _t_fmadd_pd( r03, rcosx1, r01 );
            r06 = _t_fmsub_pd( r02, rcosx0, r00 );
            r07 = _t_fmsub_pd( r03, rcosx1, r01 );
            #else
            r02 = _t_mul_pd( r02, rcosx0 );
            r03 = _t_mul_pd( r03, rcosx1 );
            
            r04 = _t_add_pd( r02, r00 );
            r05 = _t_add_pd( r03, r01 );
            r06 = _t_sub_pd( r02, r00 );
            r07 = _t_sub_pd( r03, r01 );
            #endif
            
            _t_store_pd( psumN + vlen0, r04 );
            _t_store_pd( psumN + vlen1, r05 );
            _t_store_pd( psumS + vlen0, r06 );
            _t_store_pd( psumS + vlen1, r07 );
            
            r00 = _t_load_pd( pswork1 + vlen2 );
            r01 = _t_load_pd( pswork1 + vlen3 );
            r02 = _t_load_pd( pswork2 + vlen2 );
            r03 = _t_load_pd( pswork2 + vlen3 );
            
            #if defined (__FMA__)
            r04 = _t_fmadd_pd( r02, rcosx2, r00 );
            r05 = _t_fmadd_pd( r03, rcosx3, r01 );
            r06 = _t_fmsub_pd( r02, rcosx2, r00 );
            r07 = _t_fmsub_pd( r03, rcosx3, r01 );
            #else
            r02 = _t_mul_pd( r02, rcosx2 );
            r03 = _t_mul_pd( r03, rcosx3 );
            
            r04 = _t_add_pd( r02, r00 );
            r05 = _t_add_pd( r03, r01 );
            r06 = _t_sub_pd( r02, r00 );
            r07 = _t_sub_pd( r03, r01 );
            #endif
            
            _t_store_pd( psumN + vlen2, r04 );
            _t_store_pd( psumN + vlen3, r05 );
            _t_store_pd( psumS + vlen2, r06 );
            _t_store_pd( psumS + vlen3, r07 );
            
            psumN   += vlen4 * n;
            psumS   += vlen4 * n;
            pswork1 += vlen4;
            pswork2 += vlen4;
            
        }
        
    }
    
}