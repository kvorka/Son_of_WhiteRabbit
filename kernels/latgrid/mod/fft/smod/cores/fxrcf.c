#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void fxrc0_c( const int m,
                       double *restrict arr1,
                       double *restrict arr2 )

{
    
    // Temporal variables
    double add;
    
    // Main loop
    #pragma omp unroll (vlen4) simd aligned (arr1,arr2:alignement)
    for ( int i = 0; i < vlen4 * m; i++ ) {
        
        add     = arr1[i];
        arr1[i] = arr1[i] + arr2[i];
        arr2[i] = add     - arr2[i];
        
    }
    
}

extern inline __attribute__((always_inline))
void fxrcf_c( const double sign,
              const double scale,
              const int m,
              const double *restrict t,
                    double *restrict x11,
                    double *restrict x12,
                    double *restrict x21,
                    double *restrict x22 )
{
    
    // Constants
    const double t1 = t[0];
    const double t2 = t[1];
    
    // Temporal variables
    double x1, x2, x3, x4, addre, subre, addim, subim;
    
    // Main loop
    #pragma omp unroll (vlen2) simd uniform (t1,t2,scale,sign) aligned (x11,x12,x21,x22:alignement)
    for ( int i = 0; i < vlen4 * m; i++ ) {
        
        x1 = x11[i];
        x2 = x21[i];
        
        addre = x1 + x2;
        subre = x1 - x2;
        
        x3 = x12[i];
        x4 = x22[i];
        
        addim = ( x3 + x4 );
        subim = ( x3 - x4 ) * sign;
        
        x1 = ( addre - sign * ( subre * t2 + addim * t1 ) ) * scale;
        x2 = ( subim -        ( addim * t2 - subre * t1 ) ) * scale;
        
        x11[i] = x1;
        x12[i] = x2;
        
        x3 = -x1 + ( 2 * scale ) * addre;
        x4 = +x2 - ( 2 * scale ) * subim;
        
        x21[i] = x3;
        x22[i] = x4;
        
    }
    
}