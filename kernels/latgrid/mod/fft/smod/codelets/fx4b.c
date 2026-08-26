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
    __m256d r00, r01, r02, r03, 
            r04, r05, r06, r07,
            r08, r09;
    
    for ( int i3 = 0; i3 < l4; i3++ ) {
            
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r08 = _mm256_load_pd( px0re );
                r09 = _mm256_load_pd( px0im );
                
                r04 = _mm256_load_pd( px2re );
                r05 = _mm256_load_pd( px2im );
                
                r00 = _mm256_add_pd( r08, r04 );
                r01 = _mm256_add_pd( r09, r05 );
                
                r04 = _mm256_sub_pd( r08, r04 );
                r05 = _mm256_sub_pd( r09, r05 );
                
                r08 = _mm256_load_pd( px1re );
                r09 = _mm256_load_pd( px1im );
                
                r06 = _mm256_load_pd( px3re );
                r07 = _mm256_load_pd( px3im );
                
                r02 = _mm256_add_pd( r08, r06 );
                r03 = _mm256_add_pd( r09, r07 );
                
                r06 = _mm256_sub_pd( r08, r06 );
                r07 = _mm256_sub_pd( r09, r07 );
                
                r02 = _mm256_sub_pd( r00, r02 );
                r03 = _mm256_sub_pd( r01, r03 );
                
                _mm256_store_pd( px2re, r02 );
                _mm256_store_pd( px2im, r03 );
                
                r00 = _mm256_add_pd( r00, r00 );
                r01 = _mm256_add_pd( r01, r01 );
                
                r07 = _mm256_sub_pd( r04, r07 );
                r06 = _mm256_add_pd( r05, r06 );
                
                _mm256_store_pd( px1re, r07 );
                _mm256_store_pd( px1im, r06 );
                
                r00 = _mm256_sub_pd( r00, r02 );
                r01 = _mm256_sub_pd( r01, r03 );
                
                r04 = _mm256_add_pd( r04, r04 );
                r05 = _mm256_add_pd( r05, r05 );
                
                _mm256_store_pd( px0re, r00 );
                _mm256_store_pd( px0im, r01 );
                
                r04 = _mm256_sub_pd( r04, r07 );
                r05 = _mm256_sub_pd( r05, r06 );
                
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
    __m512d r00, r01, r02, r03, r04, 
            r05, r06, r07, r08, r09;
    
    for ( int i3 = 0; i3 < l4; i3++ ) {
            
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r08 = _mm512_load_pd( px0re );
                r09 = _mm512_load_pd( px0im );
                
                r04 = _mm512_load_pd( px2re );
                r05 = _mm512_load_pd( px2im );
                
                r00 = _mm512_add_pd( r08, r04 );
                r01 = _mm512_add_pd( r09, r05 );
                
                r04 = _mm512_sub_pd( r08, r04 );
                r05 = _mm512_sub_pd( r09, r05 );
                
                r08 = _mm512_load_pd( px1re );
                r09 = _mm512_load_pd( px1im );
                
                r06 = _mm512_load_pd( px3re );
                r07 = _mm512_load_pd( px3im );
                
                r02 = _mm512_add_pd( r08, r06 );
                r03 = _mm512_add_pd( r09, r07 );
                
                r06 = _mm512_sub_pd( r08, r06 );
                r07 = _mm512_sub_pd( r09, r07 );
                
                r02 = _mm512_sub_pd( r00, r02 );
                r03 = _mm512_sub_pd( r01, r03 );
                
                _mm512_store_pd( px2re, r02 );
                _mm512_store_pd( px2im, r03 );
                
                r00 = _mm512_add_pd( r00, r00 );
                r01 = _mm512_add_pd( r01, r01 );
                
                r07 = _mm512_sub_pd( r04, r07 );
                r06 = _mm512_add_pd( r05, r06 );
                
                _mm512_store_pd( px1re, r07 );
                _mm512_store_pd( px1im, r06 );
                
                r00 = _mm512_sub_pd( r00, r02 );
                r01 = _mm512_sub_pd( r01, r03 );
                
                r04 = _mm512_add_pd( r04, r04 );
                r05 = _mm512_add_pd( r05, r05 );
                
                _mm512_store_pd( px0re, r00 );
                _mm512_store_pd( px0im, r01 );
                
                r04 = _mm512_sub_pd( r04, r07 );
                r05 = _mm512_sub_pd( r05, r06 );
                
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
    
}
#endif