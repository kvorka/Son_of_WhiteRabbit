#include <stddef.h>
#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxzm4a_c( const int m,
               const int k,
               const int l,
                     double *restrict x,
               const double *restrict t )

#if defined ( mem32 )
{
    
    // FFT adjustement
    const int l4 = l / 4;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 16 * m;
    const ptrdiff_t step2 = 16 * m * 6 * l4;
    
    // Memory addresses
    double *px0re = x + step * ( 0 + 2 * l4 * 0 );
    double *px0im = x + step * ( 1 + 2 * l4 * 0 );
    double *px1re = x + step * ( 0 + 2 * l4 * 1 );
    double *px1im = x + step * ( 1 + 2 * l4 * 1 );
    double *px2re = x + step * ( 0 + 2 * l4 * 2 );
    double *px2im = x + step * ( 1 + 2 * l4 * 2 );
    double *px3re = x + step * ( 0 + 2 * l4 * 3 );
    double *px3im = x + step * ( 1 + 2 * l4 * 3 );
    
    // Registers to be used
    __m256d rt1re, rt1im, rt2re, rt2im, r0re, r0im, r1re, r1im, r2re, r2im, r3re, r3im, r01, r02;
    
    for ( int i4 = 0; i4 < k; i4++ ) {
        
        rt1re = _mm256_broadcast_sd( t + 0 + 6 * i4 );
        rt1im = _mm256_broadcast_sd( t + 1 + 6 * i4 );
        rt2re = _mm256_broadcast_sd( t + 2 + 6 * i4 );
        rt2im = _mm256_broadcast_sd( t + 3 + 6 * i4 );
        
        for ( int i3 = 0; i3 < l4; i3++ ) {
            
            for ( int i2 = 0; i2 < m; i2++ ) {
                
                for ( int i1 = 0; i1 < 4; i1++ ) {
                    
                    r0re = _mm256_load_pd( px0re );
                    r0im = _mm256_load_pd( px0im );
                    r01  = _mm256_load_pd( px2re );
                    r02  = _mm256_load_pd( px2im );
                    
                    #if defined (__FMA__)
                    r2re = _mm256_fnmadd_pd( rt2re, r01, r0re );
                    r2im = _mm256_fnmadd_pd( rt2im, r01, r0im );
                    
                    r0re = _mm256_add_pd( r0re, r0re );
                    r0im = _mm256_add_pd( r0im, r0im );
                    
                    r2re = _mm256_fmadd_pd(  rt2im, r02, r2re );
                    r2im = _mm256_fnmadd_pd( rt2re, r02, r2im );
                    #else
                    r2re = _mm256_mul_pd( rt2re, r01 );
                    r2im = _mm256_mul_pd( rt2im, r01 );
                    r1re = _mm256_mul_pd( rt2im, r02 );
                    r1im = _mm256_mul_pd( rt2re, r02 );
                    
                    r2re = _mm256_sub_pd( r0re, r2re );
                    r2im = _mm256_sub_pd( r0im, r2im );
                    
                    r0re = _mm256_add_pd( r0re, r0re );
                    r0im = _mm256_add_pd( r0im, r0im );
                    r2re = _mm256_add_pd( r2re, r1re );
                    r2im = _mm256_sub_pd( r2im, r1im );
                    #endif
                    
                    r0re = _mm256_sub_pd( r0re, r2re );
                    r0im = _mm256_sub_pd( r0im, r2im );
                    
                    r1re = _mm256_load_pd( px1re );
                    r1im = _mm256_load_pd( px1im );
                    r01  = _mm256_load_pd( px3re );
                    r02  = _mm256_load_pd( px3im );
                    
                    #if defined (__FMA__)
                    r3re = _mm256_fnmadd_pd( rt2re, r01, r1re );
                    r3im = _mm256_fnmadd_pd( rt2im, r01, r1im );
                    
                    r1re = _mm256_add_pd( r1re, r1re );
                    r1im = _mm256_add_pd( r1im, r1im );
                    
                    r3re = _mm256_fmadd_pd(  rt2im, r02, r3re );
                    r3im = _mm256_fnmadd_pd( rt2re, r02, r3im );
                    #else
                    r3re = _mm256_mul_pd( rt2re, r01 );
                    r3im = _mm256_mul_pd( rt2im, r01 );
                    r01  = _mm256_mul_pd( rt2im, r02 );
                    r02  = _mm256_mul_pd( rt2re, r02 );
                    
                    r3re = _mm256_sub_pd( r1re, r3re );
                    r3im = _mm256_sub_pd( r1im, r3im );
                    
                    r1re = _mm256_add_pd( r1re, r1re );
                    r1im = _mm256_add_pd( r1im, r1im );
                    r3re = _mm256_add_pd( r3re, r01  );
                    r3im = _mm256_sub_pd( r3im, r02  );
                    #endif
                    
                    r1re = _mm256_sub_pd( r1re, r3re );
                    r1im = _mm256_sub_pd( r1im, r3im );
                    
                    #if defined (__FMA__)
                    r01 = _mm256_fnmadd_pd( rt1re, r1re, r0re );
                    r02 = _mm256_fnmadd_pd( rt1im, r1re, r0im );
                    
                    r01 = _mm256_fmadd_pd(  rt1im, r1im, r01 );
                    r02 = _mm256_fnmadd_pd( rt1re, r1im, r02 );
                    #else
                    r01  = _mm256_mul_pd( rt1re, r1re );
                    r02  = _mm256_mul_pd( rt1im, r1re );
                    
                    r01  = _mm256_sub_pd( r0re, r01 );
                    r02  = _mm256_sub_pd( r0im, r02 );
                    r1re = _mm256_mul_pd( rt1im, r1im );
                    r1im = _mm256_mul_pd( rt1re, r1im );
                    
                    r01 = _mm256_add_pd( r01, r1re );
                    r02 = _mm256_sub_pd( r02, r1im );
                    #endif
                    
                    r0re = _mm256_add_pd( r0re, r0re );
                    r0im = _mm256_add_pd( r0im, r0im );
                    
                    _mm256_store_pd( px2re, r01 );
                    _mm256_store_pd( px2im, r02 );
                    
                    r0re = _mm256_sub_pd( r0re, r01 );
                    r0im = _mm256_sub_pd( r0im, r02 );
                    
                    _mm256_store_pd( px0re, r0re );
                    _mm256_store_pd( px0im, r0im );
                    
                    #if defined (__FMA__)
                    r1re = _mm256_fnmadd_pd( rt1re, r3im, r2re );
                    r1im = _mm256_fmadd_pd(  rt1re, r3re, r2im );
                    
                    r2re = _mm256_add_pd( r2re, r2re );
                    r2im = _mm256_add_pd( r2im, r2im );
                    
                    r1re = _mm256_fnmadd_pd( rt1im, r3re, r1re );
                    r1im = _mm256_fnmadd_pd( rt1im, r3im, r1im );
                    #else
                    r1re = _mm256_mul_pd( rt1re, r3im );
                    r1im = _mm256_mul_pd( rt1re, r3re );
                    r01  = _mm256_mul_pd( rt1im, r3re );
                    r02  = _mm256_mul_pd( rt1im, r3im );
                    
                    r1re = _mm256_sub_pd( r2re, r1re );
                    r1im = _mm256_add_pd( r2im, r1im );
                    
                    r2re = _mm256_add_pd( r2re, r2re );
                    r2im = _mm256_add_pd( r2im, r2im );
                    r1re = _mm256_sub_pd( r1re, r01 );
                    r1im = _mm256_sub_pd( r1im, r02 );
                    #endif
                    
                    _mm256_store_pd( px1re, r1re );
                    _mm256_store_pd( px1im, r1im );
                    
                    r2re = _mm256_sub_pd( r2re, r1re );
                    r2im = _mm256_sub_pd( r2im, r1im );
                    
                    _mm256_store_pd( px3re, r2re );
                    _mm256_store_pd( px3im, r2im );
                    
                    // Walking to next SIMD line before next
                    // i1 cycle iteration.
                    px0re += 4;
                    px0im += 4;
                    px1re += 4;
                    px1im += 4;
                    px2re += 4;
                    px2im += 4;
                    px3re += 4;
                    px3im += 4;
                    
                }
                
                // No walking needed in here, because basic simd line,
                // i1 and even i2 are contiguous in memory.
                
            }
            
            // After i2 cycle, the address offset is already step, meaning 
            // px0re is where px0im initially started. Another move in addresses 
            // is required in order to move to next real/imag pair.
            px0re += step;
            px0im += step;
            px1re += step;
            px1im += step;
            px2re += step;
            px2im += step;
            px3re += step;
            px3im += step;
            
        }
        
        // After i3 cycle, the address offset is 2*l4*step. Overall step before
        // next stage needed is 8*l4*step, therefore more walking.
        px0re += step2;
        px0im += step2;
        px1re += step2;
        px1im += step2;
        px2re += step2;
        px2im += step2;
        px3re += step2;
        px3im += step2;
        
    }
    
}
#else
{
    
    // FFT adjustement
    const int l4 = l / 4;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 32 * m;
    const ptrdiff_t step2 = 32 * m * 6 * l4;
    
    // Memory addresses
    double *px0re = x + step * ( 0 + 2 * l4 * 0 );
    double *px0im = x + step * ( 1 + 2 * l4 * 0 );
    double *px1re = x + step * ( 0 + 2 * l4 * 1 );
    double *px1im = x + step * ( 1 + 2 * l4 * 1 );
    double *px2re = x + step * ( 0 + 2 * l4 * 2 );
    double *px2im = x + step * ( 1 + 2 * l4 * 2 );
    double *px3re = x + step * ( 0 + 2 * l4 * 3 );
    double *px3im = x + step * ( 1 + 2 * l4 * 3 );
    
    // Registers to be used
    __m512d rt1re, rt1im, rt2re, rt2im, r0re, r0im, r1re, r1im, r2re, r2im, r3re, r3im, r01, r02;
    
    for ( int i4 = 0; i4 < k; i4++ ) {
        
        rt1re = _mm512_set1_pd( *( t + 0 + 6 * i4 ) );
        rt1im = _mm512_set1_pd( *( t + 1 + 6 * i4 ) );
        rt2re = _mm512_set1_pd( *( t + 2 + 6 * i4 ) );
        rt2im = _mm512_set1_pd( *( t + 3 + 6 * i4 ) );
        
        for ( int i3 = 0; i3 < l4; i3++ ) {
            
            for ( int i2 = 0; i2 < m; i2++ ) {
                
                for ( int i1 = 0; i1 < 4; i1++ ) {
                    
                    r0re = _mm512_load_pd( px0re );
                    r0im = _mm512_load_pd( px0im );
                    r01  = _mm512_load_pd( px2re );
                    r02  = _mm512_load_pd( px2im );
                    
                    r2re = _mm512_fnmadd_pd( rt2re, r01, r0re );
                    r2im = _mm512_fnmadd_pd( rt2im, r01, r0im );
                    
                    r0re = _mm512_add_pd( r0re, r0re );
                    r0im = _mm512_add_pd( r0im, r0im );
                    
                    r2re = _mm512_fmadd_pd(  rt2im, r02, r2re );
                    r2im = _mm512_fnmadd_pd( rt2re, r02, r2im );
                    
                    r0re = _mm512_sub_pd( r0re, r2re );
                    r0im = _mm512_sub_pd( r0im, r2im );
                    
                    r1re = _mm512_load_pd( px1re );
                    r1im = _mm512_load_pd( px1im );
                    r01  = _mm512_load_pd( px3re );
                    r02  = _mm512_load_pd( px3im );
                    
                    r3re = _mm512_fnmadd_pd( rt2re, r01, r1re );
                    r3im = _mm512_fnmadd_pd( rt2im, r01, r1im );
                    
                    r1re = _mm512_add_pd( r1re, r1re );
                    r1im = _mm512_add_pd( r1im, r1im );
                    
                    r3re = _mm512_fmadd_pd(  rt2im, r02, r3re );
                    r3im = _mm512_fnmadd_pd( rt2re, r02, r3im );
                    
                    r1re = _mm512_sub_pd( r1re, r3re );
                    r1im = _mm512_sub_pd( r1im, r3im );
                    
                    r01 = _mm512_fnmadd_pd( rt1re, r1re, r0re );
                    r02 = _mm512_fnmadd_pd( rt1im, r1re, r0im );
                    
                    r01 = _mm512_fmadd_pd(  rt1im, r1im, r01 );
                    r02 = _mm512_fnmadd_pd( rt1re, r1im, r02 );
                    
                    r0re = _mm512_add_pd( r0re, r0re );
                    r0im = _mm512_add_pd( r0im, r0im );
                    
                    _mm512_store_pd( px2re, r01 );
                    _mm512_store_pd( px2im, r02 );
                    
                    r0re = _mm512_sub_pd( r0re, r01 );
                    r0im = _mm512_sub_pd( r0im, r02 );
                    
                    _mm512_store_pd( px0re, r0re );
                    _mm512_store_pd( px0im, r0im );
                    
                    r1re = _mm512_fnmadd_pd( rt1re, r3im, r2re );
                    r1im = _mm512_fmadd_pd(  rt1re, r3re, r2im );
                    
                    r2re = _mm512_add_pd( r2re, r2re );
                    r2im = _mm512_add_pd( r2im, r2im );
                    
                    r1re = _mm512_fnmadd_pd( rt1im, r3re, r1re );
                    r1im = _mm512_fnmadd_pd( rt1im, r3im, r1im );
                    
                    _mm512_store_pd( px1re, r1re );
                    _mm512_store_pd( px1im, r1im );
                    
                    r2re = _mm512_sub_pd( r2re, r1re );
                    r2im = _mm512_sub_pd( r2im, r1im );
                    
                    _mm512_store_pd( px3re, r2re );
                    _mm512_store_pd( px3im, r2im );
                    
                    // Walking to next SIMD line before next
                    // i1 cycle iteration.
                    px0re += 8;
                    px0im += 8;
                    px1re += 8;
                    px1im += 8;
                    px2re += 8;
                    px2im += 8;
                    px3re += 8;
                    px3im += 8;
                    
                }
                
                // No walking needed in here, because basic simd line,
                // i1 and even i2 are contiguous in memory.
                
            }
            
            // After i2 cycle, the address offset is already step, meaning 
            // px0re is where px0im initially started. Another move in addresses 
            // is required in order to move to next real/imag pair.
            px0re += step;
            px0im += step;
            px1re += step;
            px1im += step;
            px2re += step;
            px2im += step;
            px3re += step;
            px3im += step;
            
        }
        
        // After i3 cycle, the address offset is 2*l4*step. Overall step before
        // next stage needed is 8*l4*step, therefore more walking.
        px0re += step2;
        px0im += step2;
        px1re += step2;
        px1im += step2;
        px2re += step2;
        px2im += step2;
        px3re += step2;
        px3im += step2;
        
    }
    
}
#endif