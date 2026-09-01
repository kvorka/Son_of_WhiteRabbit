#include <stddef.h>
#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxzm4b_c( const int m,
               const int l,
                     double *restrict x )

#if defined ( mem32 )
{
    
    // FFT adjustement
    const int l4 = l / 4;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 16 * m;
    
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
    __m256d r0re, r0im, r1re, r1im, r2re, r2im, r3re, r3im, r01, r02, r03, r04;
    
    for ( int i3 = 0; i3 < l4; i3++ ) {
            
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r01 = _mm256_load_pd( px0re );
                r03 = _mm256_load_pd( px2re );
                
                r0re = _mm256_add_pd( r01, r03 );
                r2re = _mm256_sub_pd( r01, r03 );
                
                r02 = _mm256_load_pd( px0im );
                r04 = _mm256_load_pd( px2im );
                
                r0im = _mm256_add_pd( r02, r04 );
                r2im = _mm256_sub_pd( r02, r04 );
                
                r01 = _mm256_load_pd( px1re );
                r03 = _mm256_load_pd( px3re );
                
                r1re = _mm256_add_pd( r01, r03 );
                r3re = _mm256_sub_pd( r01, r03 );
                
                r02 = _mm256_load_pd( px1im );
                r04 = _mm256_load_pd( px3im );
                
                r1im = _mm256_add_pd( r02, r04 );
                r3im = _mm256_sub_pd( r02, r04 );
                
                r1re = _mm256_sub_pd( r0re, r1re );
                r1im = _mm256_sub_pd( r0im, r1im );
                r01  = _mm256_add_pd( r0re, r0re );
                r02  = _mm256_add_pd( r0im, r0im );
                
                _mm256_store_pd( px2re, r1re );
                _mm256_store_pd( px2im, r1im );
                
                r0re = _mm256_sub_pd( r01,  r1re );
                r0im = _mm256_sub_pd( r02,  r1im );
                r3im = _mm256_sub_pd( r2re, r3im );
                r3re = _mm256_add_pd( r2im, r3re );
                
                r01  = _mm256_add_pd( r2re, r2re );
                r02  = _mm256_add_pd( r2im, r2im );
                
                _mm256_store_pd( px0re, r0re );
                _mm256_store_pd( px0im, r0im );
                _mm256_store_pd( px1re, r3im );
                _mm256_store_pd( px1im, r3re );
                
                r2re = _mm256_sub_pd( r01, r3im );
                r2im = _mm256_sub_pd( r02, r3re );
                
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
    
}
#else
{
    
    // FFT adjustement
    const int l4 = l / 4;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 32 * m;
    
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
    __m512d r0re, r0im, r1re, r1im, r2re, r2im, r3re, r3im, r01, r02, r03, r04;
    
    for ( int i3 = 0; i3 < l4; i3++ ) {
            
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r01 = _mm512_load_pd( px0re );
                r03 = _mm512_load_pd( px2re );
                
                r0re = _mm512_add_pd( r01, r03 );
                r2re = _mm512_sub_pd( r01, r03 );
                
                r02 = _mm512_load_pd( px0im );
                r04 = _mm512_load_pd( px2im );
                
                r0im = _mm512_add_pd( r02, r04 );
                r2im = _mm512_sub_pd( r02, r04 );
                
                r01 = _mm512_load_pd( px1re );
                r03 = _mm512_load_pd( px3re );
                
                r1re = _mm512_add_pd( r01, r03 );
                r3re = _mm512_sub_pd( r01, r03 );
                
                r02 = _mm512_load_pd( px1im );
                r04 = _mm512_load_pd( px3im );
                
                r1im = _mm512_add_pd( r02, r04 );
                r3im = _mm512_sub_pd( r02, r04 );
                
                r1re = _mm512_sub_pd( r0re, r1re );
                r1im = _mm512_sub_pd( r0im, r1im );
                r01  = _mm512_add_pd( r0re, r0re );
                r02  = _mm512_add_pd( r0im, r0im );
                
                _mm512_store_pd( px2re, r1re );
                _mm512_store_pd( px2im, r1im );
                
                r0re = _mm512_sub_pd( r01,  r1re );
                r0im = _mm512_sub_pd( r02,  r1im );
                r3im = _mm512_sub_pd( r2re, r3im );
                r3re = _mm512_add_pd( r2im, r3re );
                
                r01  = _mm512_add_pd( r2re, r2re );
                r02  = _mm512_add_pd( r2im, r2im );
                
                _mm512_store_pd( px0re, r0re );
                _mm512_store_pd( px0im, r0im );
                _mm512_store_pd( px1re, r3im );
                _mm512_store_pd( px1im, r3re );
                
                r2re = _mm512_sub_pd( r01, r3im );
                r2im = _mm512_sub_pd( r02, r3re );
                
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
    
}
#endif