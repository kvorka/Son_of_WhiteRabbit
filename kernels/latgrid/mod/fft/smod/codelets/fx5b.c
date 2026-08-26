#include <stddef.h>
#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxzm5b_c( const int m,
               const int l,
                     double *restrict x )

#if defined ( mem32 )
{
    
    // FFT adjustement
    const int l5 = l / 5;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 16 * m;
    
    // Memory addresses
    double *px0re = x + step * ( 0 + 2 * l5 * 0 );
    double *px0im = x + step * ( 1 + 2 * l5 * 0 );
    double *px1re = x + step * ( 0 + 2 * l5 * 1 );
    double *px1im = x + step * ( 1 + 2 * l5 * 1 );
    double *px2re = x + step * ( 0 + 2 * l5 * 2 );
    double *px2im = x + step * ( 1 + 2 * l5 * 2 );
    double *px3re = x + step * ( 0 + 2 * l5 * 3 );
    double *px3im = x + step * ( 1 + 2 * l5 * 3 );
    double *px4re = x + step * ( 0 + 2 * l5 * 4 );
    double *px4im = x + step * ( 1 + 2 * l5 * 4 );
    
    // FFT constants
    const __m256d rC51 = _mm256_set1_pd( +0.2500000000000000000 );
    const __m256d rC52 = _mm256_set1_pd( +0.5590169943749474241 );
    const __m256d rC53 = _mm256_set1_pd( +0.6180339887498948482 );
    const __m256d rC54 = _mm256_set1_pd( -0.9510565162951535721 );
    
    // Registers to be used
    __m256d r00, r01, r02, r03, 
            r04, r05, r06, r07,
            r08, r09, r10, r11;
    
    for ( int i3 = 0; i3 < l5; i3++ ) {
            
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r04 = _mm256_load_pd( px4re );
                r05 = _mm256_load_pd( px4im );
                
                r02 = _mm256_load_pd( px1re );
                r03 = _mm256_load_pd( px1im );
                
                r00 = _mm256_sub_pd( r02, r04 );
                r01 = _mm256_sub_pd( r03, r05 );
                
                r02 = _mm256_add_pd( r02, r04 );
                r03 = _mm256_add_pd( r03, r05 );
                
                r04 = _mm256_load_pd( px2re );
                r05 = _mm256_load_pd( px2im );
                
                r08 = _mm256_load_pd( px3re );
                r09 = _mm256_load_pd( px3im );
                
                r06 = _mm256_sub_pd( r08, r04 );
                r07 = _mm256_sub_pd( r09, r05 );
                
                r08 = _mm256_add_pd( r08, r04 );
                r09 = _mm256_add_pd( r09, r05 );
                
                #if defined (fma)
                r04 = _mm256_fmadd_pd( rC53, r06, r00 );
                r05 = _mm256_fmadd_pd( rC53, r07, r01 );
                
                r06 = _mm256_fmsub_pd( rC53, r00, r06 );
                r07 = _mm256_fmsub_pd( rC53, r01, r07 );
                #else
                r04 = _mm256_mul_pd( rC53, r06 );
                r05 = _mm256_mul_pd( rC53, r07 );
                
                r04 = _mm256_add_pd( r00, r04 );
                r05 = _mm256_add_pd( r01, r05 );
                
                r00 = _mm256_mul_pd( rC53, r00 );
                r01 = _mm256_mul_pd( rC53, r01 );
                
                r06 = _mm256_sub_pd( r00, r06 );
                r07 = _mm256_sub_pd( r01, r07 );
                #endif
                
                r00 = _mm256_add_pd( r02, r08 );
                r01 = _mm256_add_pd( r03, r09 );
                
                r02 = _mm256_sub_pd( r02, r08 );
                r03 = _mm256_sub_pd( r03, r09 );
                
                r08 = _mm256_load_pd( px0re );
                r09 = _mm256_load_pd( px0im );
                
                #if defined (fma)
                r08 = _mm256_fnmadd_pd( rC51, r00, r08 );
                r09 = _mm256_fnmadd_pd( rC51, r01, r09 );
                
                r02 = _mm256_fnmadd_pd( rC52, r02, r08 );
                r03 = _mm256_fnmadd_pd( rC52, r03, r09 );
                #else
                r10 = _mm256_mul_pd( rC51, r00 );
                r11 = _mm256_mul_pd( rC51, r01 );
                
                r08 = _mm256_sub_pd( r08, r10 );
                r09 = _mm256_sub_pd( r09, r11 );
                
                r02 = _mm256_mul_pd( rC52, r02 );
                r03 = _mm256_mul_pd( rC52, r03 );
                
                r02 = _mm256_sub_pd( r08, r02 );
                r03 = _mm256_sub_pd( r09, r03 );
                #endif
                
                r08 = _mm256_add_pd( r08, r08 );
                r09 = _mm256_add_pd( r09, r09 );
                
                r08 = _mm256_sub_pd( r08, r02 );
                r09 = _mm256_sub_pd( r09, r03 );
                
                r00 = _mm256_add_pd( _mm256_load_pd( px0re ), r00 );
                r01 = _mm256_add_pd( _mm256_load_pd( px0im ), r01 );
                
                _mm256_store_pd( px0re, r00 );
                _mm256_store_pd( px0im, r01 );
                
                #if defined (fma)
                r00 = _mm256_fnmadd_pd( rC54, r07, r02 );
                r01 = _mm256_fmadd_pd(  rC54, r06, r03 );
                #else
                r00 = _mm256_mul_pd( rC54, r07 );
                r01 = _mm256_mul_pd( rC54, r06 );
                
                r00 = _mm256_sub_pd( r02, r00 );
                r01 = _mm256_add_pd( r03, r01 );
                #endif
                
                _mm256_store_pd( px3re, r00 );
                _mm256_store_pd( px3im, r01 );
                
                r02 = _mm256_add_pd( r02, r02 );
                r03 = _mm256_add_pd( r03, r03 );
                
                r02 = _mm256_sub_pd( r02, r00 );
                r03 = _mm256_sub_pd( r03, r01 );
                
                _mm256_store_pd( px2re, r02 );
                _mm256_store_pd( px2im, r03 );
                
                #if defined (fma)
                r00 = _mm256_fnmadd_pd( rC54, r05, r08 );
                r01 = _mm256_fmadd_pd(  rC54, r04, r09 );
                #else
                r00 = _mm256_mul_pd( rC54, r05 );
                r01 = _mm256_mul_pd( rC54, r04 );
                
                r00 = _mm256_sub_pd( r08, r00 );
                r01 = _mm256_add_pd( r09, r01 );
                #endif
                
                _mm256_store_pd( px4re, r00 );
                _mm256_store_pd( px4im, r01 );
                
                r08 = _mm256_add_pd( r08, r08 );
                r09 = _mm256_add_pd( r09, r09 );
                
                r08 = _mm256_sub_pd( r08, r00 );
                r09 = _mm256_sub_pd( r09, r01 );
                
                _mm256_store_pd( px1re, r08 );
                _mm256_store_pd( px1im, r09 );
                
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
                px4re += 4;
                px4im += 4;
                
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
    
}
#else
{
    
    // FFT adjustement
    const int l5 = l / 5;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 32 * m;
    
    // Memory addresses
    double *px0re = x + step * ( 0 + 2 * l5 * 0 );
    double *px0im = x + step * ( 1 + 2 * l5 * 0 );
    double *px1re = x + step * ( 0 + 2 * l5 * 1 );
    double *px1im = x + step * ( 1 + 2 * l5 * 1 );
    double *px2re = x + step * ( 0 + 2 * l5 * 2 );
    double *px2im = x + step * ( 1 + 2 * l5 * 2 );
    double *px3re = x + step * ( 0 + 2 * l5 * 3 );
    double *px3im = x + step * ( 1 + 2 * l5 * 3 );
    double *px4re = x + step * ( 0 + 2 * l5 * 4 );
    double *px4im = x + step * ( 1 + 2 * l5 * 4 );
    
    // FFT constants
    const __m512d rC51 = _mm512_set1_pd( +0.2500000000000000000 );
    const __m512d rC52 = _mm512_set1_pd( +0.5590169943749474241 );
    const __m512d rC53 = _mm512_set1_pd( +0.6180339887498948482 );
    const __m512d rC54 = _mm512_set1_pd( -0.9510565162951535721 );
    
    // Registers to be used
    __m512d r00, r01, r02, r03, 
            r04, r05, r06, r07,
            r08, r09, r10, r11;
    
    for ( int i3 = 0; i3 < l5; i3++ ) {
            
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r04 = _mm512_load_pd( px4re );
                r05 = _mm512_load_pd( px4im );
                
                r02 = _mm512_load_pd( px1re );
                r03 = _mm512_load_pd( px1im );
                
                r00 = _mm512_sub_pd( r02, r04 );
                r01 = _mm512_sub_pd( r03, r05 );
                
                r02 = _mm512_add_pd( r02, r04 );
                r03 = _mm512_add_pd( r03, r05 );
                
                r04 = _mm512_load_pd( px2re );
                r05 = _mm512_load_pd( px2im );
                
                r08 = _mm512_load_pd( px3re );
                r09 = _mm512_load_pd( px3im );
                
                r06 = _mm512_sub_pd( r08, r04 );
                r07 = _mm512_sub_pd( r09, r05 );
                
                r08 = _mm512_add_pd( r08, r04 );
                r09 = _mm512_add_pd( r09, r05 );
                
                r04 = _mm512_fmadd_pd( rC53, r06, r00 );
                r05 = _mm512_fmadd_pd( rC53, r07, r01 );
                
                r06 = _mm512_fmsub_pd( rC53, r00, r06 );
                r07 = _mm512_fmsub_pd( rC53, r01, r07 );
                
                r00 = _mm512_add_pd( r02, r08 );
                r01 = _mm512_add_pd( r03, r09 );
                
                r02 = _mm512_sub_pd( r02, r08 );
                r03 = _mm512_sub_pd( r03, r09 );
                
                r08 = _mm512_load_pd( px0re );
                r09 = _mm512_load_pd( px0im );
                
                r08 = _mm512_fnmadd_pd( rC51, r00, r08 );
                r09 = _mm512_fnmadd_pd( rC51, r01, r09 );
                
                r02 = _mm512_fnmadd_pd( rC52, r02, r08 );
                r03 = _mm512_fnmadd_pd( rC52, r03, r09 );
                
                r08 = _mm512_add_pd( r08, r08 );
                r09 = _mm512_add_pd( r09, r09 );
                
                r08 = _mm512_sub_pd( r08, r02 );
                r09 = _mm512_sub_pd( r09, r03 );
                
                r00 = _mm512_add_pd( _mm512_load_pd( px0re ), r00 );
                r01 = _mm512_add_pd( _mm512_load_pd( px0im ), r01 );
                
                _mm512_store_pd( px0re, r00 );
                _mm512_store_pd( px0im, r01 );
                
                r00 = _mm512_fnmadd_pd( rC54, r07, r02 );
                r01 = _mm512_fmadd_pd(  rC54, r06, r03 );
                
                _mm512_store_pd( px3re, r00 );
                _mm512_store_pd( px3im, r01 );
                
                r02 = _mm512_add_pd( r02, r02 );
                r03 = _mm512_add_pd( r03, r03 );
                
                r02 = _mm512_sub_pd( r02, r00 );
                r03 = _mm512_sub_pd( r03, r01 );
                
                _mm512_store_pd( px2re, r02 );
                _mm512_store_pd( px2im, r03 );
                
                r00 = _mm512_fnmadd_pd( rC54, r05, r08 );
                r01 = _mm512_fmadd_pd(  rC54, r04, r09 );
                
                _mm512_store_pd( px4re, r00 );
                _mm512_store_pd( px4im, r01 );
                
                r08 = _mm512_add_pd( r08, r08 );
                r09 = _mm512_add_pd( r09, r09 );
                
                r08 = _mm512_sub_pd( r08, r00 );
                r09 = _mm512_sub_pd( r09, r01 );
                
                _mm512_store_pd( px1re, r08 );
                _mm512_store_pd( px1im, r09 );
                
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
                px4re += 8;
                px4im += 8;
                
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
    
}
#endif