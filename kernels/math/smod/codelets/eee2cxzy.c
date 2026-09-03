#include <stddef.h>
#include <complex.h>

extern inline __attribute__((always_inline))
void eee2xyz_c( const int n,
                const double complex *restrict sumPTP,
                      double complex *restrict cc )
{
    
    // Casting memory addresses
    const double *restrict psum = ( const double * ) sumPTP;
          double *restrict pcc  = (       double * ) cc;
    
    // Addresses to be used (keep in mind complex == 2 doubles)
    const double *restrict psum1 = psum + 0*n;
    const double *restrict psum2 = psum + 2*n;
    const double *restrict psum3 = psum + 4*n;
    
    // 1 / sqrt(2)
    const double fac = 0.7071067811865475;
    
    // Temporal variables
    double s11, s12, s21, s22, c11, c12, c21, c22;
    
    // Main loop
    #if defined ( mem16 )
    #pragma omp unroll partial (2) simd uniform (fac)
    #elif defined ( mem32 )
    #pragma omp unroll partial (4) simd uniform (fac)
    #elif defined ( mem64 )
    #pragma omp unroll partial (8) simd uniform (fac)
    #endif
    for ( int i = 0; i < n; i++ ) {
        
        s11 = psum1[0+2*i];
        s12 = psum1[1+2*i];
        
        s21 = psum3[0+2*i];
        s22 = psum3[1+2*i];
        
        c11 = s11 - s21;
        c12 = s12 - s22;
        c21 = s11 + s21;
        c22 = s12 + s22;
        
        c11 = c11 * fac;
        c12 = c12 * fac;
        c21 = c21 * fac;
        c22 = c22 * fac;
        
        pcc[0 + 6*i] = +c11;
        pcc[1 + 6*i] = +c12;
        pcc[2 + 6*i] = +c22;
        pcc[3 + 6*i] = -c21;
        pcc[4 + 6*i] = psum2[0+2*i];
        pcc[5 + 6*i] = psum2[1+2*i];
        
    }
    
}