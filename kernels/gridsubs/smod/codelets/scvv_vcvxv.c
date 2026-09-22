#include "../../../math/cvec.h"

extern inline __attribute__((always_inline))
void scvv_vcvxv_c( const double *restrict gtmp,
                         double *restrict grid )

{
    
    // Number of grid points handled
    const int ngp = vlen * 4;
    
    // Memory addresses
    const double *restrict vvx = gtmp + 0*ngp;
    const double *restrict vvy = gtmp + 1*ngp;
    const double *restrict vvz = gtmp + 2*ngp;
    const double *restrict gtx = gtmp + 3*ngp;
    const double *restrict gty = gtmp + 4*ngp;
    const double *restrict gtz = gtmp + 5*ngp;
    const double *restrict xvx = gtmp + 6*ngp;
    const double *restrict xvy = gtmp + 7*ngp;
    const double *restrict xvz = gtmp + 8*ngp;
    
    double *restrict gout1 = grid + 0*ngp;
    double *restrict gout2 = grid + 1*ngp;
    double *restrict gout3 = grid + 2*ngp;
    double *restrict gout4 = grid + 3*ngp;
    
    // Registers to be used
    __td rvx, rvy, rvz, rgx, rgy, rgz, rcx, rcy, 
         rcz, rg1, rg2, rg3, rg4, r01, r02, r03;
    
    // Cycle over the 4 Legendre roots handled at once: inside, the v*gradT
    // is computed and stored into rg1 and vxcurl(v) is computed and stored
    // into rg2, rg3 and rg4
    for ( int i = 0; i < 4; i++ ) {
        
        rvx = _t_load_pd( vvx + i*vlen );
        rvy = _t_load_pd( vvy + i*vlen );
        rvz = _t_load_pd( vvz + i*vlen );
        
        rgx = _t_load_pd( gtx + i*vlen );
        rgy = _t_load_pd( gty + i*vlen );
        rgz = _t_load_pd( gtz + i*vlen );
        
        rcx = _t_load_pd( xvx + i*vlen );
        rcy = _t_load_pd( xvy + i*vlen );
        rcz = _t_load_pd( xvz + i*vlen );
        
        rg1 = _t_mul_pd( rvx, rgx );
        rg2 = _t_mul_pd( rvz, rcy );
        rg3 = _t_mul_pd( rvx, rcz );
        rg4 = _t_mul_pd( rvy, rcx );
        
        #if defined (__FMA__)
        rg1 = _t_fmadd_pd(  rvy, rgy, rg1 );
        rg2 = _t_fnmadd_pd( rvy, rcz, rg2 );
        rg3 = _t_fnmadd_pd( rvz, rcx, rg3 );
        rg4 = _t_fnmadd_pd( rvx, rcy, rg4 );
        
        rg1 = _t_fmadd_pd( rvz, rgz, rg1 );
        #else
        rgx = _t_mul_pd( rvy, rgy );
        r01 = _t_mul_pd( rvy, rcz );
        r02 = _t_mul_pd( rvz, rcx );
        r03 = _t_mul_pd( rvx, rcy );
        
        rg1 = _t_add_pd( rg1, rgx );
        rg2 = _t_sub_pd( rg2, r01 );
        rg3 = _t_sub_pd( rg3, r02 );
        rg4 = _t_sub_pd( rg4, r03 );
        
        rgy = _t_mul_pd( rvz, rgz );
        
        rg1 = _t_add_pd( rg1, rgy );
        #endif
        
        _t_store_pd( gout1 + i*vlen, rg1 );
        _t_store_pd( gout2 + i*vlen, rg2 );
        _t_store_pd( gout3 + i*vlen, rg3 );
        _t_store_pd( gout4 + i*vlen, rg4 );
        
    }
    
}