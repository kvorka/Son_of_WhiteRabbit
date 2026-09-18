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
    const double *restrict pswork2 = swork + 8 * vlen * n;
          double *restrict psumN;
          double *restrict psumS;
    
    // cosine registers
    const __td rcosx0 = _t_load_pd( cosx + 0*vlen );
    const __td rcosx1 = _t_load_pd( cosx + 1*vlen );
    const __td rcosx2 = _t_load_pd( cosx + 2*vlen );
    const __td rcosx3 = _t_load_pd( cosx + 3*vlen );
    
    // other registers to be used
    __td r00, r01, r02, r03, r04, r05, r06, r07;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + 4 * vlen * i3;
        psumS = sumS + 4 * vlen * i3;
        
        for ( int i2 = 0; i2 < 2; i2++ ) {
            
            r00 = _t_load_pd( pswork1 + 0*vlen );
            r01 = _t_load_pd( pswork1 + 1*vlen );
            r02 = _t_load_pd( pswork2 + 0*vlen );
            r03 = _t_load_pd( pswork2 + 1*vlen );
            
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
            
            _t_store_pd( psumN + 0*vlen, r04 );
            _t_store_pd( psumN + 1*vlen, r05 );
            _t_store_pd( psumS + 0*vlen, r06 );
            _t_store_pd( psumS + 1*vlen, r07 );
            
            r00 = _t_load_pd( pswork1 + 2*vlen );
            r01 = _t_load_pd( pswork1 + 3*vlen );
            r02 = _t_load_pd( pswork2 + 2*vlen );
            r03 = _t_load_pd( pswork2 + 3*vlen );
            
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
            
            _t_store_pd( psumN + 2*vlen, r04 );
            _t_store_pd( psumN + 3*vlen, r05 );
            _t_store_pd( psumS + 2*vlen, r06 );
            _t_store_pd( psumS + 3*vlen, r07 );
            
            psumN   += 4 * vlen * n;
            psumS   += 4 * vlen * n;
            pswork1 += 4 * vlen;
            pswork2 += 4 * vlen;
            
        }
        
    }
    
}