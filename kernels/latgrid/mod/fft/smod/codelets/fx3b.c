#include <stddef.h>
#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxzm3b_c( const int m,
               const int l,
                     double *restrict x )

#if defined ( mem32 )
{
    
    // FFT adjustement
    const int l3 = l / 3;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step = 16 * m;
    
    // Memory addresses
    double *px0re = x + step * ( 0 + 2 * l3 * 0 );
    double *px0im = x + step * ( 1 + 2 * l3 * 0 );
    double *px1re = x + step * ( 0 + 2 * l3 * 1 );
    double *px1im = x + step * ( 1 + 2 * l3 * 1 );
    double *px2re = x + step * ( 0 + 2 * l3 * 2 );
    double *px2im = x + step * ( 1 + 2 * l3 * 2 );
    
    // FFT constants
    const __m256d rTwo = _mm256_set1_pd( +2.00000000000000000000 );
    const __m256d rC31 = _mm256_set1_pd( -0.50000000000000000000 );
    const __m256d rC32 = _mm256_set1_pd( +0.86602540378443864676 );
    
    // Registers to be used
    __m256d r00, r01, r02, r03, 
            r04, r05, r06, r07,
            r08, r09, r10, r11;
    
    for ( int i3 = 0; i3 < l3; i3++ ) {
        
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r06 = _mm256_load_pd( px1re );
                r07 = _mm256_load_pd( px1im );
                r04 = _mm256_load_pd( px2re );
                r05 = _mm256_load_pd( px2im );
                
                r02 = _mm256_sub_pd( r06, r04 );
                r03 = _mm256_sub_pd( r07, r05 );
                r00 = _mm256_add_pd( r06, r04 );
                r01 = _mm256_add_pd( r07, r05 );
                
                r06 = _mm256_load_pd( px0re );
                r07 = _mm256_load_pd( px0im );
                
                #if defined (fma)
                r04 = _mm256_fmadd_pd( rC31, r00, r06 );
                r05 = _mm256_fmadd_pd( rC31, r01, r07 );
                #else
                r08 = _mm256_mul_pd( rC31, r00 );
                r09 = _mm256_mul_pd( rC31, r01 );
                
                r04 = _mm256_add_pd( r08, r06 );
                r05 = _mm256_add_pd( r09, r07 );
                #endif
                
                r00 = _mm256_add_pd( r00, r06 );
                r01 = _mm256_add_pd( r01, r07 );
                
                _mm256_store_pd( px0re, r00 );
                _mm256_store_pd( px0im, r01 );
                
                #if defined (fma)
                r03 = _mm256_fmadd_pd(  rC32, r03, r04 );
                r02 = _mm256_fnmadd_pd( rC32, r02, r05 );
                #else
                r03 = _mm256_mul_pd( rC32, r03 );
                r02 = _mm256_mul_pd( rC32, r02 );
                
                r03 = _mm256_add_pd( r04, r03 );
                r02 = _mm256_sub_pd( r05, r02 );
                #endif
                
                _mm256_store_pd( px2re, r03 );
                _mm256_store_pd( px2im, r02 );
                
                #if defined (fma)
                r04 = _mm256_fmsub_pd( rTwo, r04, r03 );
                r05 = _mm256_fmsub_pd( rTwo, r05, r02 );
                #else
                r04 = _mm256_add_pd( r04, r04 );
                r05 = _mm256_add_pd( r05, r05 );
                
                r04 = _mm256_sub_pd( r04, r03 );
                r05 = _mm256_sub_pd( r05, r02 );
                #endif
                
                _mm256_store_pd( px1re, r04 );
                _mm256_store_pd( px1im, r05 );
                
                // Walking to next SIMD line before next
                // i1 cycle iteration.
                px0re += 4;
                px0im += 4;
                px1re += 4;
                px1im += 4;
                px2re += 4;
                px2im += 4;
                
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
        
    }
    
}
#else
{
    
    // FFT adjustement
    const int l3 = l / 3;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step = 32 * m;
    
    // Memory addresses
    double *px0re = x + step * ( 0 + 2 * l3 * 0 );
    double *px0im = x + step * ( 1 + 2 * l3 * 0 );
    double *px1re = x + step * ( 0 + 2 * l3 * 1 );
    double *px1im = x + step * ( 1 + 2 * l3 * 1 );
    double *px2re = x + step * ( 0 + 2 * l3 * 2 );
    double *px2im = x + step * ( 1 + 2 * l3 * 2 );
    
    // FFT constants
    const __m512d rTwo = _mm512_set1_pd( +2.00000000000000000000 );
    const __m512d rC31 = _mm512_set1_pd( -0.50000000000000000000 );
    const __m512d rC32 = _mm512_set1_pd( +0.86602540378443864676 );
    
    // Registers to be used
    __m512d r00, r01, r02, r03, 
            r04, r05, r06, r07,
            r08, r09, r10, r11;
    
    for ( int i3 = 0; i3 < l3; i3++ ) {
        
        for ( int i2 = 0; i2 < m; i2++ ) {
            
            for ( int i1 = 0; i1 < 4; i1++ ) {
                
                r06 = _mm512_load_pd( px1re );
                r07 = _mm512_load_pd( px1im );
                r04 = _mm512_load_pd( px2re );
                r05 = _mm512_load_pd( px2im );
                
                r02 = _mm512_sub_pd( r06, r04 );
                r03 = _mm512_sub_pd( r07, r05 );
                r00 = _mm512_add_pd( r06, r04 );
                r01 = _mm512_add_pd( r07, r05 );
                
                r06 = _mm512_load_pd( px0re );
                r07 = _mm512_load_pd( px0im );
                
                r04 = _mm512_fmadd_pd( rC31, r00, r06 );
                r05 = _mm512_fmadd_pd( rC31, r01, r07 );
                
                r00 = _mm512_add_pd( r00, r06 );
                r01 = _mm512_add_pd( r01, r07 );
                
                _mm512_store_pd( px0re, r00 );
                _mm512_store_pd( px0im, r01 );
                
                r03 = _mm512_fmadd_pd(  rC32, r03, r04 );
                r02 = _mm512_fnmadd_pd( rC32, r02, r05 );
                
                _mm512_store_pd( px2re, r03 );
                _mm512_store_pd( px2im, r02 );
                
                r04 = _mm512_fmsub_pd( rTwo, r04, r03 );
                r05 = _mm512_fmsub_pd( rTwo, r05, r02 );
                
                _mm512_store_pd( px1re, r04 );
                _mm512_store_pd( px1im, r05 );
                
                // Walking to next SIMD line before next
                // i1 cycle iteration.
                px0re += 8;
                px0im += 8;
                px1re += 8;
                px1im += 8;
                px2re += 8;
                px2im += 8;
                
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
        
    }
    
}
#endif