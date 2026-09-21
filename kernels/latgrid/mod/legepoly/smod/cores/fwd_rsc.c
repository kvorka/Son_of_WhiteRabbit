#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void fwd_rsc_c( const int n,
                const double *restrict w,
                const double *restrict cosx,
                const double *restrict sumN,
                const double *restrict sumS,
                      double *restrict swork )

{
    
    // memory pointers
    const double *restrict psumN;
    const double *restrict psumS;
            
    double *restrict pswork1 = swork;
    double *restrict pswork2 = swork + 8 * vlen * n;
    
    // weights
    const __td rw0 = _t_load_pd( w + 0*vlen );
    const __td rw1 = _t_load_pd( w + 1*vlen );
    const __td rw2 = _t_load_pd( w + 2*vlen );
    const __td rw3 = _t_load_pd( w + 3*vlen );
    
    // cosines * weights
    const __td rcw0 = _t_mul_pd( rw0, _t_load_pd( cosx + 0*vlen ) );
    const __td rcw1 = _t_mul_pd( rw1, _t_load_pd( cosx + 1*vlen ) );
    const __td rcw2 = _t_mul_pd( rw2, _t_load_pd( cosx + 2*vlen ) );
    const __td rcw3 = _t_mul_pd( rw3, _t_load_pd( cosx + 3*vlen ) );
    
    // other registers to be used
    __td r00, r01, r10, r11;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + 4 * vlen * i3;
        psumS = sumS + 4 * vlen * i3;
        
        for ( int i2 = 0; i2 < 2; i2++ ) {
            
            r00 = _t_load_pd( psumN );
            r01 = _t_load_pd( psumS );
            
            r10 = _t_sub_pd( r00, r01 );
            r11 = _t_add_pd( r00, r01 );
            
            r10 = _t_mul_pd(  rw0, r10 );
            r11 = _t_mul_pd( rcw0, r11 );
            
            _t_store_pd( pswork1, r10 );
            _t_store_pd( pswork2, r11 );
            
            r00 = _t_load_pd( psumN + vlen );
            r01 = _t_load_pd( psumS + vlen );
            
            r10 = _t_sub_pd( r00, r01 );
            r11 = _t_add_pd( r00, r01 );
            
            r10 = _t_mul_pd(  rw1, r10 );
            r11 = _t_mul_pd( rcw1, r11 );
            
            _t_store_pd( pswork1 + vlen, r10 );
            _t_store_pd( pswork2 + vlen, r11 );
            
            r00 = _t_load_pd( psumN + 2*vlen );
            r01 = _t_load_pd( psumS + 2*vlen );
            
            r10 = _t_sub_pd( r00, r01 );
            r11 = _t_add_pd( r00, r01 );
            
            r10 = _t_mul_pd(  rw2, r10 );
            r11 = _t_mul_pd( rcw2, r11 );
            
            _t_store_pd( pswork1 + 2*vlen, r10 );
            _t_store_pd( pswork2 + 2*vlen, r11 );
            
            r00 = _t_load_pd( psumN + 3*vlen );
            r01 = _t_load_pd( psumS + 3*vlen );
            
            r10 = _t_sub_pd( r00, r01 );
            r11 = _t_add_pd( r00, r01 );
            
            r10 = _t_mul_pd(  rw3, r10 );
            r11 = _t_mul_pd( rcw3, r11 );
            
            _t_store_pd( pswork1 + 3*vlen, r10 );
            _t_store_pd( pswork2 + 3*vlen, r11 );
            
            psumN   += 4 * vlen * n;
            psumS   += 4 * vlen * n;
            pswork1 += 4 * vlen;
            pswork2 += 4 * vlen;
            
        }
        
    }
    
}