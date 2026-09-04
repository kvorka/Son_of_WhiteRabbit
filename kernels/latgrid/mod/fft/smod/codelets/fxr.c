#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxr2c_c( const int m,
              const double *restrict t,
                    double *restrict x11,
                    double *restrict x12,
                    double *restrict x21,
                    double *restrict x22 )

{
    
    // Constants
    const double t1 = *( t + 0 );
    const double t2 = *( t + 1 );
    
    // Memory addresses
    double *px11 = x11;
    double *px12 = x12;
    double *px21 = x21;
    double *px22 = x22;
    
    // Temporal variables
    double addre, subre, addim, subim;
    
    // Main loop
    #if defined ( mem32 )
    #pragma omp unroll (8) simd uniform (t1,t2) aligned (x11,x12,x21,x22:32)
    for ( int i = 0; i < 16*m; i++ ) {
        
        addre = x11[i] + x21[i];
        subre = x11[i] - x21[i];
        addim = x22[i] + x12[i];
        subim = x22[i] - x12[i];
        
        x11[i] = ( addre + subre * t2 + addim * t1 ) / 2;
        x12[i] = ( subim - addim * t2 + subre * t1 ) / 2;
        
        x21[i] = -x11[i] + addre;
        x22[i] = +x12[i] - subim;
        
    }
    #else
    #pragma omp unroll (8) simd uniform (t1,t2) aligned (x11,x12,x21,x22:64)
    for ( int i = 0; i < 32*m; i++ ) {
        
        addre = x11[i] + x21[i];
        subre = x11[i] - x21[i];
        addim = x22[i] + x12[i];
        subim = x22[i] - x12[i];
        
        x11[i] = ( addre + subre * t2 + addim * t1 ) / 2;
        x12[i] = ( subim - addim * t2 + subre * t1 ) / 2;
        
        x21[i] = -x11[i] + addre;
        x22[i] = +x12[i] - subim;
        
    }
    #endif
    
}