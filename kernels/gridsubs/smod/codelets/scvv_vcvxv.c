#include "../../../math/cvec.h"

extern inline __attribute__((always_inline))
void scvv_vcvxv_c( const double *restrict gtmp,
                         double *restrict grid )

{
    
    // Memory addresses
    const double *restrict vx  = gtmp + 0*vlen4;
    const double *restrict vy  = gtmp + 1*vlen4;
    const double *restrict vz  = gtmp + 2*vlen4;
    const double *restrict gtx = gtmp + 3*vlen4;
    const double *restrict gty = gtmp + 4*vlen4;
    const double *restrict gtz = gtmp + 5*vlen4;
    const double *restrict xvx = gtmp + 6*vlen4;
    const double *restrict xvy = gtmp + 7*vlen4;
    const double *restrict xvz = gtmp + 8*vlen4;
    
    double *restrict g1 = grid + 0*vlen4;
    double *restrict g2 = grid + 1*vlen4;
    double *restrict g3 = grid + 2*vlen4;
    double *restrict g4 = grid + 3*vlen4;
    
    // Main cycle
    #pragma omp unroll (vlen) simd aligned (g1,g2,g3,g4,vx,vy,vz,gtx,gty,gtz,xvx,xvy,xvz:alignement)
    for ( int i = 0; i < vlen4; i++ ) {
        
        g1[i] = vx[i] * gtx[i] + vy[i] * gty[i] + vz[i] * gtz[i];
        g2[i] = vz[i] * xvy[i] - vy[i] * xvz[i];
        g3[i] = vx[i] * xvz[i] - vz[i] * xvx[i];
        g4[i] = vy[i] * xvx[i] - vx[i] * xvy[i];
        
    }
    
}