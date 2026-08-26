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
    __m256d rt1re, rt1im,
            rt2re, rt2im,
            r00, r01, r02, r03,
            r04, r05, r06, r07,
            r08, r09, r10;
    
    for ( int i4 = 0; i4 < k; i4++ ) {
        
        rt1re = _mm256_broadcast_sd( t + 0 + 6 * i4 );
        rt1im = _mm256_broadcast_sd( t + 1 + 6 * i4 );
        rt2re = _mm256_broadcast_sd( t + 2 + 6 * i4 );
        rt2im = _mm256_broadcast_sd( t + 3 + 6 * i4 );
        
        for ( int i3 = 0; i3 < l4; i3++ ) {
            
            for ( int i2 = 0; i2 < m; i2++ ) {
                
                for ( int i1 = 0; i1 < 4; i1++ ) {
                    
                    r00 = _mm256_load_pd( px0re );
                    r01 = _mm256_load_pd( px0im );
                    r04 = _mm256_load_pd( px2re );
                    r05 = _mm256_load_pd( px2im );
                    
                    #if defined (fma)
                    r08 = _mm256_fnmadd_pd( rt2re, r04, r00 );
                    r09 = _mm256_fnmadd_pd( rt2im, r04, r01 );
                    #else
                    r08 = _mm256_mul_pd( rt2re, r04 );
                    r09 = _mm256_mul_pd( rt2im, r04 );
                    
                    r08 = _mm256_sub_pd( r00, r08 );
                    r09 = _mm256_sub_pd( r01, r09 );
                    #endif
                    
                    r00 = _mm256_add_pd( r00, r00 );
                    r01 = _mm256_add_pd( r01, r01 );
                    
                    #if defined (fma)
                    r04 = _mm256_fmadd_pd(  rt2im, r05, r08 );
                    r05 = _mm256_fnmadd_pd( rt2re, r05, r09 );
                    #else
                    r04 = _mm256_mul_pd( rt2im, r05 );
                    r05 = _mm256_mul_pd( rt2re, r05 );
                    
                    r04 = _mm256_add_pd( r08, r04 );
                    r05 = _mm256_sub_pd( r09, r05 );
                    #endif
                    
                    r00 = _mm256_sub_pd( r00, r04 );
                    r01 = _mm256_sub_pd( r01, r05 );
                    
                    r02 = _mm256_load_pd( px1re );
                    r03 = _mm256_load_pd( px1im );
                    r06 = _mm256_load_pd( px3re );
                    r07 = _mm256_load_pd( px3im );
                    
                    #if defined (fma)
                    r08 = _mm256_fnmadd_pd( rt2re, r06, r02 );
                    r09 = _mm256_fnmadd_pd( rt2im, r06, r03 );
                    #else
                    r08 = _mm256_mul_pd( rt2re, r06 );
                    r09 = _mm256_mul_pd( rt2im, r06 );
                    
                    r08 = _mm256_sub_pd( r02, r08 );
                    r09 = _mm256_sub_pd( r03, r09 );
                    #endif
                    
                    r02 = _mm256_add_pd( r02, r02 );
                    r03 = _mm256_add_pd( r03, r03 );
                    
                    #if defined (fma)
                    r06 = _mm256_fmadd_pd(  rt2im, r07, r08 );
                    r07 = _mm256_fnmadd_pd( rt2re, r07, r09 );
                    #else
                    r06 = _mm256_mul_pd( rt2im, r07 );
                    r10 = _mm256_mul_pd( rt2re, r07 );
                    
                    r06 = _mm256_add_pd( r08, r06 );
                    r07 = _mm256_sub_pd( r09, r10 );
                    #endif
                    
                    r02 = _mm256_sub_pd( r02, r06 );
                    r03 = _mm256_sub_pd( r03, r07 );
                    
                    #if defined (fma)
                    r08 = _mm256_fnmadd_pd( rt1re, r02, r00 );
                    r09 = _mm256_fnmadd_pd( rt1im, r02, r01 );
                    #else
                    r08 = _mm256_mul_pd( rt1re, r02 );
                    r09 = _mm256_mul_pd( rt1im, r02 );
                    
                    r08 = _mm256_sub_pd( r00, r08 );
                    r09 = _mm256_sub_pd( r01, r09 );
                    #endif
                    
                    r00 = _mm256_add_pd( r00, r00 );
                    r01 = _mm256_add_pd( r01, r01 );
                    
                    #if defined (fma)
                    r02 = _mm256_fmadd_pd(  rt1im, r03, r08 );
                    r03 = _mm256_fnmadd_pd( rt1re, r03, r09 );
                    #else
                    r02 = _mm256_mul_pd( rt1im, r03 );
                    r10 = _mm256_mul_pd( rt1re, r03 );
                    
                    r02 = _mm256_add_pd( r08, r02 );
                    r03 = _mm256_sub_pd( r09, r10 );
                    #endif
                    
                    _mm256_store_pd( px2re, r02 );
                    _mm256_store_pd( px2im, r03 );
                    
                    r00 = _mm256_sub_pd( r00, r02 );
                    r01 = _mm256_sub_pd( r01, r03 );
                    
                    _mm256_store_pd( px0re, r00 );
                    _mm256_store_pd( px0im, r01 );
                    
                    #if defined (fma)
                    r08 = _mm256_fnmadd_pd( rt1re, r07, r04 );
                    r09 = _mm256_fmadd_pd(  rt1re, r06, r05 );
                    #else
                    r08 = _mm256_mul_pd( rt1re, r07 );
                    r09 = _mm256_mul_pd( rt1re, r06 );
                    
                    r08 = _mm256_sub_pd( r04, r08 );
                    r09 = _mm256_add_pd( r05, r09 );
                    #endif
                    
                    r04 = _mm256_add_pd( r04, r04 );
                    r05 = _mm256_add_pd( r05, r05 );
                    
                    #if defined (fma)
                    r02 = _mm256_fnmadd_pd( rt1im, r06, r08 );
                    r03 = _mm256_fnmadd_pd( rt1im, r07, r09 );
                    #else
                    r02 = _mm256_mul_pd( rt1im, r06 );
                    r03 = _mm256_mul_pd( rt1im, r07 );
                    
                    r02 = _mm256_sub_pd( r08, r02 );
                    r03 = _mm256_sub_pd( r09, r03 );
                    #endif
                    
                    _mm256_store_pd( px1re, r02 );
                    _mm256_store_pd( px1im, r03 );
                    
                    r04 = _mm256_sub_pd( r04, r02 );
                    r05 = _mm256_sub_pd( r05, r03 );
                    
                    _mm256_store_pd( px3re, r04 );
                    _mm256_store_pd( px3im, r05 );
                    
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
    __m512d rt1re, rt1im, 
            rt2re, rt2im,
            r00, r01, r02, r03, r04, 
            r05, r06, r07, r08, r09;
    
    for ( int i4 = 0; i4 < k; i4++ ) {
        
        rt1re = _mm512_set1_pd( *( t + 0 + 6 * i4 ) );
        rt1im = _mm512_set1_pd( *( t + 1 + 6 * i4 ) );
        rt2re = _mm512_set1_pd( *( t + 2 + 6 * i4 ) );
        rt2im = _mm512_set1_pd( *( t + 3 + 6 * i4 ) );
        
        for ( int i3 = 0; i3 < l4; i3++ ) {
            
            for ( int i2 = 0; i2 < m; i2++ ) {
                
                for ( int i1 = 0; i1 < 4; i1++ ) {
                    
                    r00 = _mm512_load_pd( px0re );
                    r01 = _mm512_load_pd( px0im );
                    r04 = _mm512_load_pd( px2re );
                    r05 = _mm512_load_pd( px2im );
                    
                    r08 = _mm512_fnmadd_pd( rt2re, r04, r00 );
                    r09 = _mm512_fnmadd_pd( rt2im, r04, r01 );
                    
                    r00 = _mm512_add_pd( r00, r00 );
                    r01 = _mm512_add_pd( r01, r01 );
                    
                    r04 = _mm512_fmadd_pd(  rt2im, r05, r08 );
                    r05 = _mm512_fnmadd_pd( rt2re, r05, r09 );
                    
                    r00 = _mm512_sub_pd( r00, r04 );
                    r01 = _mm512_sub_pd( r01, r05 );
                    
                    r02 = _mm512_load_pd( px1re );
                    r03 = _mm512_load_pd( px1im );
                    r06 = _mm512_load_pd( px3re );
                    r07 = _mm512_load_pd( px3im );
                    
                    r08 = _mm512_fnmadd_pd( rt2re, r06, r02 );
                    r09 = _mm512_fnmadd_pd( rt2im, r06, r03 );
                    
                    r02 = _mm512_add_pd( r02, r02 );
                    r03 = _mm512_add_pd( r03, r03 );
                    
                    r06 = _mm512_fmadd_pd(  rt2im, r07, r08 );
                    r07 = _mm512_fnmadd_pd( rt2re, r07, r09 );
                    
                    r02 = _mm512_sub_pd( r02, r06 );
                    r03 = _mm512_sub_pd( r03, r07 );
                    
                    r08 = _mm512_fnmadd_pd( rt1re, r02, r00 );
                    r09 = _mm512_fnmadd_pd( rt1im, r02, r01 );
                    
                    r00 = _mm512_add_pd( r00, r00 );
                    r01 = _mm512_add_pd( r01, r01 );
                    
                    r02 = _mm512_fmadd_pd(  rt1im, r03, r08 );
                    r03 = _mm512_fnmadd_pd( rt1re, r03, r09 );
                    
                    _mm512_store_pd( px2re, r02 );
                    _mm512_store_pd( px2im, r03 );
                    
                    r00 = _mm512_sub_pd( r00, r02 );
                    r01 = _mm512_sub_pd( r01, r03 );
                    
                    _mm512_store_pd( px0re, r00 );
                    _mm512_store_pd( px0im, r01 );
                    
                    r08 = _mm512_fnmadd_pd( rt1re, r07, r04 );
                    r09 = _mm512_fmadd_pd(  rt1re, r06, r05 );
                    
                    r04 = _mm512_add_pd( r04, r04 );
                    r05 = _mm512_add_pd( r05, r05 );
                    
                    r02 = _mm512_fnmadd_pd( rt1im, r06, r08 );
                    r03 = _mm512_fnmadd_pd( rt1im, r07, r09 );
                    
                    _mm512_store_pd( px1re, r02 );
                    _mm512_store_pd( px1im, r03 );
                    
                    r04 = _mm512_sub_pd( r04, r02 );
                    r05 = _mm512_sub_pd( r05, r03 );
                    
                    _mm512_store_pd( px3re, r04 );
                    _mm512_store_pd( px3im, r05 );
                    
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