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
    __m256d r0re, r0im, r1re, r1im, r2re, r2im, r3re, r3im, r4re, r4im, r01, r02;
    
    for ( int i3 = 0; i3 < l5; i3++ ) {
            
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r0im = _mm256_load_pd( px1re );
                r0re = _mm256_load_pd( px4re );
                r2im = _mm256_load_pd( px1im );
                r2re = _mm256_load_pd( px4im );
                
                r1re = _mm256_add_pd( r0im, r0re );
                r4re = _mm256_sub_pd( r0im, r0re );
                
                r0im = _mm256_load_pd( px2re );
                r0re = _mm256_load_pd( px3re );
                
                r1im = _mm256_add_pd( r2im, r2re );
                r4im = _mm256_sub_pd( r2im, r2re );
                
                r2im = _mm256_load_pd( px2im );
                r2re = _mm256_load_pd( px3im );
                
                r3re = _mm256_sub_pd( r0im, r0re );
                r0re = _mm256_add_pd( r0im, r0re );
                
                r3im = _mm256_sub_pd( r2im, r2re );
                r0im = _mm256_add_pd( r2im, r2re );
                
                #if defined ( __FMA__ )
                r2re = _mm256_fmadd_pd( rC53, r3re, r4re );
                r2im = _mm256_fmadd_pd( rC53, r3im, r4im );
                
                r3re = _mm256_fmsub_pd( rC53, r4re, r3re );
                r3im = _mm256_fmsub_pd( rC53, r4im, r3im );
                #else
                r2re = _mm256_mul_pd( rC53, r3re );
                r2im = _mm256_mul_pd( rC53, r3im );
                r01  = _mm256_mul_pd( rC53, r4re );
                r02  = _mm256_mul_pd( rC53, r4im );
                
                r2re = _mm256_add_pd( r2re, r4re );
                r2im = _mm256_add_pd( r2im, r4im );
                r3re = _mm256_sub_pd( r01, r3re );
                r3im = _mm256_sub_pd( r02, r3im );
                #endif
                
                r4re = _mm256_add_pd( r1re, r0re );
                r4im = _mm256_add_pd( r1im, r0im );
                
                r1re = _mm256_sub_pd( r1re, r0re );
                r1im = _mm256_sub_pd( r1im, r0im );
                
                r0re = _mm256_load_pd( px0re );
                r0im = _mm256_load_pd( px0im );
                
                r01 = _mm256_add_pd( r0re, r4re );
                r02 = _mm256_add_pd( r0im, r4im );
                
                _mm256_store_pd( px0re, r01 );
                _mm256_store_pd( px0im, r02 );
                
                #if defined ( __FMA__ )
                r0re = _mm256_fnmadd_pd( rC51, r4re, r0re );
                r0im = _mm256_fnmadd_pd( rC51, r4im, r0im );
                
                r1re = _mm256_fnmadd_pd( rC52, r1re, r0re );
                r1im = _mm256_fnmadd_pd( rC52, r1im, r0im );
                #else
                r01  = _mm256_mul_pd( rC51, r4re );
                r02  = _mm256_mul_pd( rC51, r4im );
                r1re = _mm256_mul_pd( rC52, r1re );
                r1im = _mm256_mul_pd( rC52, r1im );
                
                r0re = _mm256_sub_pd( r0re, r01 );
                r0im = _mm256_sub_pd( r0im, r02 );
                
                r1re = _mm256_sub_pd( r0re, r1re );
                r1im = _mm256_sub_pd( r0im, r1im );
                #endif
                
                r0re = _mm256_add_pd( r0re, r0re );
                r0im = _mm256_add_pd( r0im, r0im );
                
                r0re = _mm256_sub_pd( r0re, r1re );
                r0im = _mm256_sub_pd( r0im, r1im );
                
                #if defined ( __FMA__ )
                r3re = _mm256_fmadd_pd(  rC54, r3re, r1im );
                r3im = _mm256_fnmadd_pd( rC54, r3im, r1re );
                r2im = _mm256_fnmadd_pd( rC54, r2im, r0re );
                r2re = _mm256_fmadd_pd(  rC54, r2re, r0im );
                #else
                r3re = _mm256_mul_pd( rC54, r3re );
                r3im = _mm256_mul_pd( rC54, r3im );
                r2im = _mm256_mul_pd( rC54, r2im );
                r2re = _mm256_mul_pd( rC54, r2re );
                
                r3re = _mm256_add_pd( r1im, r3re );
                r3im = _mm256_sub_pd( r1re, r3im );
                r2im = _mm256_sub_pd( r0re, r2im );
                r2re = _mm256_add_pd( r0im, r2re );
                #endif
                
                _mm256_store_pd( px4re, r2im );
                _mm256_store_pd( px4im, r2re );
                _mm256_store_pd( px3re, r3im );
                _mm256_store_pd( px3im, r3re );
                
                r1re = _mm256_add_pd( r1re, r1re );
                r1im = _mm256_add_pd( r1im, r1im );
                r0re = _mm256_add_pd( r0re, r0re );
                r0im = _mm256_add_pd( r0im, r0im );
                
                r1re = _mm256_sub_pd( r1re, r3im );
                r1im = _mm256_sub_pd( r1im, r3re );
                r0re = _mm256_sub_pd( r0re, r2im );
                r0im = _mm256_sub_pd( r0im, r2re );
                
                _mm256_store_pd( px2re, r1re );
                _mm256_store_pd( px2im, r1im );
                _mm256_store_pd( px1re, r0re );
                _mm256_store_pd( px1im, r0im );
                
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
    __m512d r0re, r0im, r1re, r1im, r2re, r2im, r3re, r3im, r4re, r4im, r01, r02, r03, r04;
    
    for ( int i3 = 0; i3 < l5; i3++ ) {
            
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r03  = _mm512_load_pd( px1re );
                r04  = _mm512_load_pd( px4re );
                r2im = _mm512_load_pd( px1im );
                r2re = _mm512_load_pd( px4im );
                
                r1re = _mm512_add_pd( r03, r04 );
                r4re = _mm512_sub_pd( r03, r04 );
                
                r03 = _mm512_load_pd( px2re );
                r04 = _mm512_load_pd( px3re );
                
                r1im = _mm512_add_pd( r2im, r2re );
                r4im = _mm512_sub_pd( r2im, r2re );
                
                r2im = _mm512_load_pd( px2im );
                r2re = _mm512_load_pd( px3im );
                
                r01  = _mm512_sub_pd( r03,  r04  );
                r0re = _mm512_add_pd( r03,  r04  );
                r02  = _mm512_sub_pd( r2im, r2re );
                r0im = _mm512_add_pd( r2im, r2re );
                
                r2re = _mm512_fmadd_pd( rC53, r01, r4re );
                r2im = _mm512_fmadd_pd( rC53, r02, r4im );
                r3re = _mm512_fmsub_pd( rC53, r4re, r01 );
                r3im = _mm512_fmsub_pd( rC53, r4im, r02 );
                
                r4re = _mm512_add_pd( r1re, r0re );
                r4im = _mm512_add_pd( r1im, r0im );
                r03  = _mm512_sub_pd( r1re, r0re );
                r04  = _mm512_sub_pd( r1im, r0im );
                
                r0re = _mm512_load_pd( px0re );
                r0im = _mm512_load_pd( px0im );
                
                r01 = _mm512_add_pd( r0re, r4re );
                r02 = _mm512_add_pd( r0im, r4im );
                
                _mm512_store_pd( px0re, r01 );
                _mm512_store_pd( px0im, r02 );
                
                r0re = _mm512_fnmadd_pd( rC51, r4re, r0re );
                r0im = _mm512_fnmadd_pd( rC51, r4im, r0im );
                
                r1re = _mm512_fnmadd_pd( rC52, r03, r0re );
                r1im = _mm512_fnmadd_pd( rC52, r04, r0im );
                
                r0re = _mm512_add_pd( r0re, r0re );
                r0im = _mm512_add_pd( r0im, r0im );
                
                r0re = _mm512_sub_pd( r0re, r1re );
                r0im = _mm512_sub_pd( r0im, r1im );
                
                r3re = _mm512_fmadd_pd(  rC54, r3re, r1im );
                r3im = _mm512_fnmadd_pd( rC54, r3im, r1re );
                r2im = _mm512_fnmadd_pd( rC54, r2im, r0re );
                r2re = _mm512_fmadd_pd(  rC54, r2re, r0im );
                
                _mm512_store_pd( px4re, r2im );
                _mm512_store_pd( px4im, r2re );
                _mm512_store_pd( px3re, r3im );
                _mm512_store_pd( px3im, r3re );
                
                r1re = _mm512_add_pd( r1re, r1re );
                r1im = _mm512_add_pd( r1im, r1im );
                r0re = _mm512_add_pd( r0re, r0re );
                r0im = _mm512_add_pd( r0im, r0im );
                
                r1re = _mm512_sub_pd( r1re, r3im );
                r1im = _mm512_sub_pd( r1im, r3re );
                r0re = _mm512_sub_pd( r0re, r2im );
                r0im = _mm512_sub_pd( r0im, r2re );
                
                _mm512_store_pd( px2re, r1re );
                _mm512_store_pd( px2im, r1im );
                _mm512_store_pd( px1re, r0re );
                _mm512_store_pd( px1im, r0im );
                
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