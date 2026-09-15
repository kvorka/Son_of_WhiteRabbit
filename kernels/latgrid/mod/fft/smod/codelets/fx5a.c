#include "../../../../../math/cvec.h"

extern inline __attribute__((always_inline))
void fxzm5a_c( const int m,
               const int k,
               const int l,
                     double *restrict x,
               const double *restrict t )

{
    
    // FFT adjustement
    const int l5 = l / 5;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 4 * vlen * m;
    const ptrdiff_t step2 = 4 * vlen * m * 8 * l5;
    
    // Memory addresses
    double *restrict px0re = x + step * ( 0 + 2 * l5 * 0 );
    double *restrict px0im = x + step * ( 1 + 2 * l5 * 0 );
    double *restrict px1re = x + step * ( 0 + 2 * l5 * 1 );
    double *restrict px1im = x + step * ( 1 + 2 * l5 * 1 );
    double *restrict px2re = x + step * ( 0 + 2 * l5 * 2 );
    double *restrict px2im = x + step * ( 1 + 2 * l5 * 2 );
    double *restrict px3re = x + step * ( 0 + 2 * l5 * 3 );
    double *restrict px3im = x + step * ( 1 + 2 * l5 * 3 );
    double *restrict px4re = x + step * ( 0 + 2 * l5 * 4 );
    double *restrict px4im = x + step * ( 1 + 2 * l5 * 4 );
    
    // FFT constants
    const __td rC51 = _t_set1_pd( +0.2500000000000000000 );
    const __td rC52 = _t_set1_pd( +0.5590169943749474241 );
    const __td rC53 = _t_set1_pd( +0.6180339887498948482 );
    const __td rC54 = _t_set1_pd( -0.9510565162951535721 );
    
    // Registers to be used
    __td rt1re, rt1im, rt2re, rt2im,
         rt3re, rt3im, rt4re, rt4im,
         r00, r01, r02, r03,
         r04, r05, r06, r07,
         r08, r09, r10, r11;
    
    for ( int i4 = 0; i4 < k; i4++ ) {
        
        rt1re = _t_set1_pd( *( t + 0 + 8 * i4 ) );
        rt1im = _t_set1_pd( *( t + 1 + 8 * i4 ) );
        rt2re = _t_set1_pd( *( t + 2 + 8 * i4 ) );
        rt2im = _t_set1_pd( *( t + 3 + 8 * i4 ) );
        rt3re = _t_set1_pd( *( t + 4 + 8 * i4 ) );
        rt3im = _t_set1_pd( *( t + 5 + 8 * i4 ) );
        rt4re = _t_set1_pd( *( t + 6 + 8 * i4 ) );
        rt4im = _t_set1_pd( *( t + 7 + 8 * i4 ) );
        
        for ( int i3 = 0; i3 < l5; i3++ ) {
            
            for ( int i2 = 0; i2 < m; i2++ ) {
                
                for ( int i1 = 0; i1 < 4; i1++ ) {
                    
                    r10 = _t_load_pd( px1re );
                    r11 = _t_load_pd( px1im );
                    
                    r02 = _t_mul_pd( rt1re, r10 );
                    r03 = _t_mul_pd( rt1re, r11 );
                    
                    #if defined ( __FMA__ ) || defined(__AVX512F__)
                    r02 = _t_fnmadd_pd( rt1im, r11, r02 );
                    r03 = _t_fmadd_pd(  rt1im, r10, r03 );
                    #else
                    r04 = _t_mul_pd( rt1im, r11 );
                    r05 = _t_mul_pd( rt1im, r10 );
                    
                    r02 = _t_sub_pd( r02, r04 );
                    r03 = _t_add_pd( r03, r05 );
                    #endif
                    
                    r10 = _t_load_pd( px2re );
                    r11 = _t_load_pd( px2im );
                    
                    r04 = _t_mul_pd( rt2re, r10 );
                    r05 = _t_mul_pd( rt2re, r11 );
                    
                    #if defined ( __FMA__ ) || defined(__AVX512F__)
                    r04 = _t_fnmadd_pd( rt2im, r11, r04 );
                    r05 = _t_fmadd_pd(  rt2im, r10, r05 );
                    #else
                    r06 = _t_mul_pd( rt2im, r11 );
                    r07 = _t_mul_pd( rt2im, r10 );
                    
                    r04 = _t_sub_pd( r04, r06 );
                    r05 = _t_add_pd( r05, r07 );
                    #endif
                    
                    r10 = _t_load_pd( px3re );
                    r11 = _t_load_pd( px3im );
                    
                    #if defined ( __FMA__ ) || defined(__AVX512F__)
                    r06 = _t_fnmadd_pd( rt3re, r10, r04 );
                    r07 = _t_fnmadd_pd( rt3re, r11, r05 );
                    
                    r06 = _t_fmadd_pd(  rt3im, r11, r06 );
                    r07 = _t_fnmadd_pd( rt3im, r10, r07 );
                    #else
                    r06 = _t_mul_pd( rt3re, r10 );
                    r07 = _t_mul_pd( rt3re, r11 );
                    
                    r06 = _t_sub_pd( r04, r06 );
                    r07 = _t_sub_pd( r05, r07 );
                    
                    r11 = _t_mul_pd( rt3im, r11 );
                    r10 = _t_mul_pd( rt3im, r10 );
                    
                    r06 = _t_add_pd( r06, r11 );
                    r07 = _t_sub_pd( r07, r10 );
                    #endif
                    
                    r10 = _t_load_pd( px4re );
                    r11 = _t_load_pd( px4im );
                    
                    #if defined ( __FMA__ ) || defined(__AVX512F__)
                    r00 = _t_fnmadd_pd( rt4re, r10, r02 );
                    r01 = _t_fnmadd_pd( rt4re, r11, r03 );
                    
                    r00 = _t_fmadd_pd(  rt4im, r11, r00 );
                    r01 = _t_fnmadd_pd( rt4im, r10, r01 );
                    #else
                    r00 = _t_mul_pd( rt4re, r10 );
                    r01 = _t_mul_pd( rt4re, r11 );
                    
                    r00 = _t_sub_pd( r02, r00 );
                    r01 = _t_sub_pd( r03, r01 );
                    
                    r11 = _t_mul_pd( rt4im, r11 );
                    r10 = _t_mul_pd( rt4im, r10 );
                    
                    r00 = _t_add_pd( r00, r11 );
                    r01 = _t_sub_pd( r01, r10 );
                    #endif
                    
                    r02 = _t_add_pd( r02, r02 );
                    r03 = _t_add_pd( r03, r03 );
                    r10 = _t_add_pd( r04, r04 );
                    r11 = _t_add_pd( r05, r05 );
                    
                    r02 = _t_sub_pd( r02, r00 );
                    r03 = _t_sub_pd( r03, r01 );
                    r08 = _t_sub_pd( r10,  r06 );
                    r09 = _t_sub_pd( r11,  r07 );
                    
                    #if defined ( __FMA__ ) || defined(__AVX512F__)
                    r04 = _t_fmadd_pd( rC53, r06, r00 );
                    r05 = _t_fmadd_pd( rC53, r07, r01 );
                    
                    r06 = _t_fmsub_pd( rC53, r00, r06 );
                    r07 = _t_fmsub_pd( rC53, r01, r07 );
                    #else
                    r04 = _t_mul_pd( rC53, r06 );
                    r05 = _t_mul_pd( rC53, r07 );
                    
                    r04 = _t_add_pd( r00, r04 );
                    r05 = _t_add_pd( r01, r05 );
                    
                    r10 = _t_mul_pd( rC53, r00 );
                    r11 = _t_mul_pd( rC53, r01 );
                    
                    r06 = _t_sub_pd( r10, r06 );
                    r07 = _t_sub_pd( r11, r07 );
                    #endif
                    
                    r00 = _t_add_pd( r02, r08 );
                    r01 = _t_add_pd( r03, r09 );
                    
                    r02 = _t_sub_pd( r02, r08 );
                    r03 = _t_sub_pd( r03, r09 );
                    
                    r10 = _t_load_pd( px0re );
                    r11 = _t_load_pd( px0im );
                    
                    #if defined ( __FMA__ ) || defined(__AVX512F__)
                    r08 = _t_fnmadd_pd( rC51, r00, r10 );
                    r09 = _t_fnmadd_pd( rC51, r01, r11 );
                    
                    r02 = _t_fnmadd_pd( rC52, r02, r08 );
                    r03 = _t_fnmadd_pd( rC52, r03, r09 );
                    #else
                    r08 = _t_mul_pd( rC51, r00 );
                    r09 = _t_mul_pd( rC51, r01 );
                    r02 = _t_mul_pd( rC52, r02 );
                    r03 = _t_mul_pd( rC52, r03 );
                    
                    r08 = _t_sub_pd( r10, r08 );
                    r09 = _t_sub_pd( r11, r09 );

                    r02 = _t_sub_pd( r08, r02 );
                    r03 = _t_sub_pd( r09, r03 );
                    #endif
                    
                    r08 = _t_add_pd( r08, r08 );
                    r09 = _t_add_pd( r09, r09 );
                    
                    r08 = _t_sub_pd( r08, r02 );
                    r09 = _t_sub_pd( r09, r03 );
                    
                    r00 = _t_add_pd( r10, r00 );
                    r01 = _t_add_pd( r11, r01 );
                    
                    _t_store_pd( px0re, r00 );
                    _t_store_pd( px0im, r01 );
                    
                    #if defined ( __FMA__ ) || defined(__AVX512F__)
                    r10 = _t_fnmadd_pd( rC54, r07, r02 );
                    r11 = _t_fmadd_pd(  rC54, r06, r03 );
                    #else
                    r10 = _t_mul_pd( rC54, r07 );
                    r11 = _t_mul_pd( rC54, r06 );
                    
                    r10 = _t_sub_pd( r02, r10 );
                    r11 = _t_add_pd( r03, r11 );
                    #endif
                    
                    _t_store_pd( px3re, r10 );
                    _t_store_pd( px3im, r11 );
                    
                    r02 = _t_add_pd( r02, r02 );
                    r03 = _t_add_pd( r03, r03 );
                    
                    #if defined ( __FMA__ ) || defined(__AVX512F__)
                    r00 = _t_fnmadd_pd( rC54, r05, r08 );
                    r01 = _t_fmadd_pd(  rC54, r04, r09 );
                    #else
                    r00 = _t_mul_pd( rC54, r05 );
                    r01 = _t_mul_pd( rC54, r04 );
                    
                    r00 = _t_sub_pd( r08, r00 );
                    r01 = _t_add_pd( r09, r01 );
                    #endif
                    
                    _t_store_pd( px4re, r00 );
                    _t_store_pd( px4im, r01 );
                    
                    r02 = _t_sub_pd( r02, r10 );
                    r03 = _t_sub_pd( r03, r11 );
                    
                    _t_store_pd( px2re, r02 );
                    _t_store_pd( px2im, r03 );
                    
                    r08 = _t_add_pd( r08, r08 );
                    r09 = _t_add_pd( r09, r09 );
                    
                    r08 = _t_sub_pd( r08, r00 );
                    r09 = _t_sub_pd( r09, r01 );
                    
                    _t_store_pd( px1re, r08 );
                    _t_store_pd( px1im, r09 );
                    
                    // Walking to next SIMD line before next
                    // i1 cycle iteration.
                    px0re += vlen;
                    px0im += vlen;
                    px1re += vlen;
                    px1im += vlen;
                    px2re += vlen;
                    px2im += vlen;
                    px3re += vlen;
                    px3im += vlen;
                    px4re += vlen;
                    px4im += vlen;
                    
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
            px4re += step;
            px4im += step;
            
        }
        
        // After i3 cycle, the address offset is 2*l5*step. Overall step before
        // next stage needed is 8*l5*step, therefore more walking.
        px0re += step2;
        px0im += step2;
        px1re += step2;
        px1im += step2;
        px2re += step2;
        px2im += step2;
        px3re += step2;
        px3im += step2;
        px4re += step2;
        px4im += step2;
        
    }
    
}