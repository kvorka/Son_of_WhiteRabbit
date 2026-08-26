#include <immintrin.h>

extern inline __attribute__((always_inline))
void fwd_rsc_c( const int n,
                const double *restrict w,
                const double *restrict cosx,
                const double *restrict sumN,
                const double *restrict sumS,
                      double *restrict swork )

#if defined ( mem32 )
{
    
    // memory pointers
    const double *psumN;
    const double *psumS;
            
    double *pswork1 = swork;
    double *pswork2 = swork + 32 * n;
    
    // weights
    const __m256d rw0 = _mm256_load_pd( w +  0 );
    const __m256d rw1 = _mm256_load_pd( w +  4 );
    const __m256d rw2 = _mm256_load_pd( w +  8 );
    const __m256d rw3 = _mm256_load_pd( w + 12 );
    
    // cosines and weights
    const __m256d rcw0 = _mm256_mul_pd( rw0, _mm256_load_pd( cosx + +  0 ) );
    const __m256d rcw1 = _mm256_mul_pd( rw1, _mm256_load_pd( cosx + +  4 ) );
    const __m256d rcw2 = _mm256_mul_pd( rw2, _mm256_load_pd( cosx + +  8 ) );
    const __m256d rcw3 = _mm256_mul_pd( rw3, _mm256_load_pd( cosx + + 12 ) );
    
    // other registers to be used
    __m256d r00, r01, r10, r11;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + 16 * i3;
        psumS = sumS + 16 * i3;
        
        for ( int i2 = 0; i2 < 2; i2++ ) {
            
            r00 = _mm256_load_pd( psumN );
            r01 = _mm256_load_pd( psumS );
            
            r10 = _mm256_sub_pd( r00, r01 );
            r11 = _mm256_add_pd( r00, r01 );
            
            r10 = _mm256_mul_pd(  rw0, r10 );
            r11 = _mm256_mul_pd( rcw0, r11 );
            
            _mm256_store_pd( pswork1, r10 );
            _mm256_store_pd( pswork2, r11 );
            
            r00 = _mm256_load_pd( psumN + 4 );
            r01 = _mm256_load_pd( psumS + 4 );
            
            r10 = _mm256_sub_pd( r00, r01 );
            r11 = _mm256_add_pd( r00, r01 );
            
            r10 = _mm256_mul_pd(  rw1, r10 );
            r11 = _mm256_mul_pd( rcw1, r11 );
            
            _mm256_store_pd( pswork1 + 4, r10 );
            _mm256_store_pd( pswork2 + 4, r11 );
            
            r00 = _mm256_load_pd( psumN + 8 );
            r01 = _mm256_load_pd( psumS + 8 );
            
            r10 = _mm256_sub_pd( r00, r01 );
            r11 = _mm256_add_pd( r00, r01 );
            
            r10 = _mm256_mul_pd(  rw2, r10 );
            r11 = _mm256_mul_pd( rcw2, r11 );
            
            _mm256_store_pd( pswork1 + 8, r10 );
            _mm256_store_pd( pswork2 + 8, r11 );
            
            r00 = _mm256_load_pd( psumN + 12 );
            r01 = _mm256_load_pd( psumS + 12 );
            
            r10 = _mm256_sub_pd( r00, r01 );
            r11 = _mm256_add_pd( r00, r01 );
            
            r10 = _mm256_mul_pd(  rw3, r10 );
            r11 = _mm256_mul_pd( rcw3, r11 );
            
            _mm256_store_pd( pswork1 + 12, r10 );
            _mm256_store_pd( pswork2 + 12, r11 );
            
            psumN   += 16 * n;
            psumS   += 16 * n;
            pswork1 += 16;
            pswork2 += 16;
            
        }
        
    }
    
}
#else
{
    
    // memory pointers
    const double *psumN;
    const double *psumS;
            
    double *pswork1 = swork;
    double *pswork2 = swork + 64 * n;
    
    // weights
    const __m512d rw0 = _mm512_load_pd( w +  0 );
    const __m512d rw1 = _mm512_load_pd( w +  8 );
    const __m512d rw2 = _mm512_load_pd( w + 16 );
    const __m512d rw3 = _mm512_load_pd( w + 24 );
    
    // cosines and weights
    const __m512d rcw0 = _mm512_mul_pd( rw0, _mm512_load_pd( cosx + +  0 ) );
    const __m512d rcw1 = _mm512_mul_pd( rw1, _mm512_load_pd( cosx + +  8 ) );
    const __m512d rcw2 = _mm512_mul_pd( rw2, _mm512_load_pd( cosx + + 16 ) );
    const __m512d rcw3 = _mm512_mul_pd( rw3, _mm512_load_pd( cosx + + 24 ) );
    
    // other registers to be used
    __m512d r00, r01, r02, r03,
            r10, r11, r12, r13;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + 32 * i3;
        psumS = sumS + 32 * i3;
        
        for ( int i2 = 0; i2 < 2; i2++ ) {
            
            r00 = _mm512_load_pd( psumN + 0 );
            r01 = _mm512_load_pd( psumS + 0 );
            r02 = _mm512_load_pd( psumN + 8 );
            r03 = _mm512_load_pd( psumS + 8 );
            
            r10 = _mm512_sub_pd( r00, r01 );
            r11 = _mm512_add_pd( r00, r01 );
            r12 = _mm512_sub_pd( r02, r03 );
            r13 = _mm512_add_pd( r02, r03 );
            
            r10 = _mm512_mul_pd(  rw0, r10 );
            r11 = _mm512_mul_pd( rcw0, r11 );
            r12 = _mm512_mul_pd(  rw1, r12 );
            r13 = _mm512_mul_pd( rcw1, r13 );
            
            _mm512_store_pd( pswork1 + 0, r10 );
            _mm512_store_pd( pswork2 + 0, r11 );
            _mm512_store_pd( pswork1 + 8, r12 );
            _mm512_store_pd( pswork2 + 8, r13 );
            
            r00 = _mm512_load_pd( psumN + 16 );
            r01 = _mm512_load_pd( psumS + 16 );
            r02 = _mm512_load_pd( psumN + 24 );
            r03 = _mm512_load_pd( psumS + 24 );
            
            r10 = _mm512_sub_pd( r00, r01 );
            r11 = _mm512_add_pd( r00, r01 );
            r12 = _mm512_sub_pd( r02, r03 );
            r13 = _mm512_add_pd( r02, r03 );
            
            r10 = _mm512_mul_pd(  rw2, r10 );
            r11 = _mm512_mul_pd( rcw2, r11 );
            r12 = _mm512_mul_pd(  rw3, r12 );
            r13 = _mm512_mul_pd( rcw3, r13 );
            
            _mm512_store_pd( pswork1 + 16, r10 );
            _mm512_store_pd( pswork2 + 16, r11 );
            _mm512_store_pd( pswork1 + 24, r12 );
            _mm512_store_pd( pswork2 + 24, r13 );
            
            psumN   += 32 * n;
            psumS   += 32 * n;
            pswork1 += 32;
            pswork2 += 32;
            
        }
        
    }
    
}
#endif