#include <immintrin.h>

extern inline __attribute__((always_inline))
void fxzm2a_c( const int m,
               const int k,
               const int l,
                     double *restrict x,
               const double *restrict t )

#if defined ( mem32 )
{
    
    // FFT adjustement
    const int l2 = l / 2;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 16 * m;
    const ptrdiff_t step2 = 16 * m * l2 * 2;
    
    // Memory addresses
    double *px0re = x + step * ( 0 + 2 * l2 * 0 );
    double *px0im = x + step * ( 1 + 2 * l2 * 0 );
    double *px1re = x + step * ( 0 + 2 * l2 * 1 );
    double *px1im = x + step * ( 1 + 2 * l2 * 1 );
    
    // Registers to be used
    __m256d rtre, rtim, 
            r00, r01, r02, r03,
            r04, r05, r06, r07;
    
    for ( int i4 = 0; i4 < k; i4++ ) {
        
        rtre = _mm256_broadcast_sd( t + 0 + 2 * i4 );
        rtim = _mm256_broadcast_sd( t + 1 + 2 * i4 );
        
        for ( int i3 = 0; i3 < l2; i3++ ) {
            
            for ( int i2 = 0; i2 < m; i2++ ) {
                
                for ( int i1 = 0; i1 < 4; i1++ ) {
                    
                    r00 = _mm256_load_pd( px0re );
                    r01 = _mm256_load_pd( px0im );
                    
                    r02 = _mm256_add_pd( r00, r00 );
                    r03 = _mm256_add_pd( r01, r01 );
                    
                    r04 = _mm256_load_pd( px1re );
                    r05 = _mm256_load_pd( px1im );
                    
                    #if defined (fma)
                    r00 = _mm256_fnmadd_pd( rtre, r04, r00 );
                    r01 = _mm256_fnmadd_pd( rtim, r04, r01 );
                    
                    r00 = _mm256_fmadd_pd(  rtim, r05, r00 );
                    r01 = _mm256_fnmadd_pd( rtre, r05, r01 );
                    #else
                    r06 = _mm256_mul_pd( rtre, r04 );
                    r07 = _mm256_mul_pd( rtim, r04 );
                    
                    r00 = _mm256_sub_pd( r00, r06 );
                    r01 = _mm256_sub_pd( r01, r07 );
                    
                    r06 = _mm256_mul_pd( rtim, r05 );
                    r07 = _mm256_mul_pd( rtre, r05 );
                    
                    r00 = _mm256_add_pd( r00, r06 );
                    r01 = _mm256_sub_pd( r01, r07 );
                    #endif
                    
                    _mm256_store_pd( px1re, r00 );
                    _mm256_store_pd( px1im, r01 );
                    
                    r00 = _mm256_sub_pd( r02, r00 );
                    r01 = _mm256_sub_pd( r03, r01 );
                    
                    _mm256_store_pd( px0re, r00 );
                    _mm256_store_pd( px0im, r01 );
                    
                    // Walking to next SIMD line before next
                    // i1 cycle iteration.
                    px0re += 4;
                    px0im += 4;
                    px1re += 4;
                    px1im += 4;
                    
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
            
        }
        
        // After i3 cycle, the address offset is 2*l2*step. Overall step before
        // next stage needed is 4*l2*step, therefore more walking.
        px0re += step2;
        px0im += step2;
        px1re += step2;
        px1im += step2;
        
    }
    
}
#else
{
    
    // FFT adjustement
    const int l2 = l / 2;
    
    // Walking pointer difference between real and imag part
    const ptrdiff_t step  = 32 * m;
    const ptrdiff_t step2 = 32 * m * l2 * 2;
    
    // Memory addresses
    double *px0re = x + step * ( 0 + 2 * l2 * 0 );
    double *px0im = x + step * ( 1 + 2 * l2 * 0 );
    double *px1re = x + step * ( 0 + 2 * l2 * 1 );
    double *px1im = x + step * ( 1 + 2 * l2 * 1 );
    
    // Registers to be used
    __m512d rtre, rtim, 
            r00, r01, r02, r03,
            r04, r05, r06, r07;
    
    for ( int i4 = 0; i4 < k; i4++ ) {
        
        rtre = _mm512_set1_pd( *( t + 0 + 2 * i4 ) );
        rtim = _mm512_set1_pd( *( t + 1 + 2 * i4 ) );
        
        for ( int i3 = 0; i3 < l2; i3++ ) {
            
            for ( int i2 = 0; i2 < m; i2++ ) {
                
                for ( int i1 = 0; i1 < 4; i1++ ) {
                    
                    r00 = _mm512_load_pd( px0re );
                    r01 = _mm512_load_pd( px0im );
                    
                    r02 = _mm512_add_pd( r00, r00 );
                    r03 = _mm512_add_pd( r01, r01 );
                    
                    r04 = _mm512_load_pd( px1re );
                    r05 = _mm512_load_pd( px1im );
                    
                    r00 = _mm512_fnmadd_pd( rtre, r04, r00 );
                    r01 = _mm512_fnmadd_pd( rtim, r04, r01 );
                    
                    r00 = _mm512_fmadd_pd(  rtim, r05, r00 );
                    r01 = _mm512_fnmadd_pd( rtre, r05, r01 );
                    
                    _mm512_store_pd( px1re, r00 );
                    _mm512_store_pd( px1im, r01 );
                    
                    r00 = _mm512_sub_pd( r02, r00 );
                    r01 = _mm512_sub_pd( r03, r01 );
                    
                    _mm512_store_pd( px0re, r00 );
                    _mm512_store_pd( px0im, r01 );
                    
                    // Walking to next SIMD line before next
                    // i1 cycle iteration.
                    px0re += 8;
                    px0im += 8;
                    px1re += 8;
                    px1im += 8;
                    
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
            
        }
        
        // After i3 cycle, the address offset is 2*l2*step. Overall step before
        // next stage needed is 4*l2*step, therefore more walking.
        px0re += step2;
        px0im += step2;
        px1re += step2;
        px1im += step2;
        
    }
    
}
#endif