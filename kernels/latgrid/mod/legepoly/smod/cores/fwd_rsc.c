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
    double *restrict pswork2 = swork + 2 * vlen4 * n;
    
    // weights
    const __td rw0 = _t_load_pd( w + vlen0 );
    const __td rw1 = _t_load_pd( w + vlen1 );
    const __td rw2 = _t_load_pd( w + vlen2 );
    const __td rw3 = _t_load_pd( w + vlen3 );
    
    // cosines * weights
    const __td rcw0 = _t_mul_pd( rw0, _t_load_pd( cosx + vlen0 ) );
    const __td rcw1 = _t_mul_pd( rw1, _t_load_pd( cosx + vlen1 ) );
    const __td rcw2 = _t_mul_pd( rw2, _t_load_pd( cosx + vlen2 ) );
    const __td rcw3 = _t_mul_pd( rw3, _t_load_pd( cosx + vlen3 ) );
    
    // other registers to be used
    __td r00, r01, r10, r11;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + vlen4 * i3;
        psumS = sumS + vlen4 * i3;
        
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
            
            r00 = _t_load_pd( psumN + vlen2 );
            r01 = _t_load_pd( psumS + vlen2 );
            
            r10 = _t_sub_pd( r00, r01 );
            r11 = _t_add_pd( r00, r01 );
            
            r10 = _t_mul_pd(  rw2, r10 );
            r11 = _t_mul_pd( rcw2, r11 );
            
            _t_store_pd( pswork1 + vlen2, r10 );
            _t_store_pd( pswork2 + vlen2, r11 );
            
            r00 = _t_load_pd( psumN + vlen3 );
            r01 = _t_load_pd( psumS + vlen3 );
            
            r10 = _t_sub_pd( r00, r01 );
            r11 = _t_add_pd( r00, r01 );
            
            r10 = _t_mul_pd(  rw3, r10 );
            r11 = _t_mul_pd( rcw3, r11 );
            
            _t_store_pd( pswork1 + vlen3, r10 );
            _t_store_pd( pswork2 + vlen3, r11 );
            
            psumN   += vlen4 * n;
            psumS   += vlen4 * n;
            pswork1 += vlen4;
            pswork2 += vlen4;
            
        }
        
    }
    
}