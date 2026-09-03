#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void xy2ee_c( const int n,
                    double complex *restrict cx,
                    double complex *restrict cy )

{
    
    // Casting memory addresses
    double *restrict px = ( double * ) cx;
    double *restrict py = ( double * ) cy;
    
    // sqrt(2)
    const double fac = 0.7071067811865475;
    
    // Temporal stores
    double xr1, xi1, yr1, yi1, yrs1, yis1;
    
    // Main loop
    #if defined ( mem16 )
    #pragma omp unroll partial (2) simd uniform (fac)
    #elif defined ( mem32 )
    #pragma omp unroll partial (4) simd uniform (fac)
    #elif defined ( mem64 )
    #pragma omp unroll partial (8) simd uniform (fac)
    #endif
    for ( int i = 0; i < 2*n; i += 2 ) {
        
        xr1 = px[i+0];
        xi1 = px[i+1];
        
        xr1 = xr1 * fac;
        xi1 = xi1 * fac;
        
        yr1 = py[i+0];
        yi1 = py[i+1];
        
        yrs1 = yi1;
        yis1 = yr1;
        
        px[i+0] = -fac * yrs1 + xr1;
        px[i+1] = +fac * yis1 + xi1;
        
        py[i+0] = -fac * yrs1 - xr1;
        py[i+1] = +fac * yis1 - xi1;
        
    }
    
}