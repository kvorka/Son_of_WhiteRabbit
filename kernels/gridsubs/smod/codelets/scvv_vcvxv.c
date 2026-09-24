#include "gcopy.h"

extern inline __attribute__((always_inline))
void scvv_vcvxv_c( const double *restrict gin,
                         double *restrict gout,
                         double *restrict gtmp )

{
    
    // At first, let us copy 9 * vlen * 4 values corresponding to the vx, vy, vz, gtx, gty, 
    // gtz, xvx, vxy, xvz (in this order by construction) for 4 * vlen latitudes.
    {
        
        gcopy_c( 9, gin, gtmp );
        
    }
    
    // The main computation of v.grad(T) and vxcurl(v)
    {
        
        #define in(row,i)  gtmp[(row) * vlen4 + (i)]
        #define out(row,i) gout[(row) * vlen4 + (i)]
        
        #pragma omp unroll (vlen) simd aligned (gtmp,gout:alignement)
        for ( int i = 0; i < vlen4; i++ ) {
            
            out(0,i) = in(0,i) * in(3,i) + in(1,i) * in(4,i) + in(2,i) * in(5,i);
            out(1,i) = in(2,i) * in(7,i) - in(1,i) * in(8,i);
            out(2,i) = in(0,i) * in(8,i) - in(2,i) * in(6,i);
            out(3,i) = in(1,i) * in(6,i) - in(0,i) * in(7,i);
            
        }
        
        #undef in
        #undef out
        
    }
    
}