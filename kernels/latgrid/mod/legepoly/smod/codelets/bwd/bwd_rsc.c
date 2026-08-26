#include <immintrin.h>

extern inline __attribute__((always_inline))
void bwd_rsc_c( const int n,
                const double *restrict cosx,
                const double *restrict swork,
                      double *restrict sumN,
                      double *restrict sumS )

#if defined ( mem32 )
{
    
    // memory pointers
    const double *pswork1 = swork;
    const double *pswork2 = swork + 32 * n;
    
    double *psumN;
    double *psumS;
    
    // cosine registers
    const __m256d rcosx0 = _mm256_load_pd( cosx +  0 );
    const __m256d rcosx1 = _mm256_load_pd( cosx +  4 );
    const __m256d rcosx2 = _mm256_load_pd( cosx +  8 );
    const __m256d rcosx3 = _mm256_load_pd( cosx + 12 );
    
    // other registers to be used
    __m256d r00, r01, r02, r03,
            r04, r05, r06, r07;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + 16 * i3;
        psumS = sumS + 16 * i3;
        
        for ( int i2 = 0; i2 < 2; i2++ ) {
            
            r00 = _mm256_load_pd( pswork1 + 0 );
            r01 = _mm256_load_pd( pswork1 + 4 );
            r02 = _mm256_load_pd( pswork2 + 0 );
            r03 = _mm256_load_pd( pswork2 + 4 );
            
            #if defined ( fma )
            r04 = _mm256_fmadd_pd( r02, rcosx0, r00 );
            r05 = _mm256_fmadd_pd( r03, rcosx1, r01 );
            r06 = _mm256_fmsub_pd( r02, rcosx0, r00 );
            r07 = _mm256_fmsub_pd( r03, rcosx1, r01 );
            #else
            r02 = _mm256_mul_pd( r02, rcosx0 );
            r03 = _mm256_mul_pd( r03, rcosx1 );
            
            r04 = _mm256_add_pd( r02, r00 );
            r05 = _mm256_add_pd( r03, r01 );
            r06 = _mm256_sub_pd( r02, r00 );
            r07 = _mm256_sub_pd( r03, r01 );
            #endif
            
            _mm256_store_pd( psumN + 0, r04 );
            _mm256_store_pd( psumN + 4, r05 );
            _mm256_store_pd( psumS + 0, r06 );
            _mm256_store_pd( psumS + 4, r07 );
            
            r00 = _mm256_load_pd( pswork1 +  8 );
            r01 = _mm256_load_pd( pswork1 + 12 );
            r02 = _mm256_load_pd( pswork2 +  8 );
            r03 = _mm256_load_pd( pswork2 + 12 );
            
            #if defined ( fma )
            r04 = _mm256_fmadd_pd( r02, rcosx2, r00 );
            r05 = _mm256_fmadd_pd( r03, rcosx3, r01 );
            r06 = _mm256_fmsub_pd( r02, rcosx2, r00 );
            r07 = _mm256_fmsub_pd( r03, rcosx3, r01 );
            #else
            r02 = _mm256_mul_pd( r02, rcosx2 );
            r03 = _mm256_mul_pd( r03, rcosx3 );
            
            r04 = _mm256_add_pd( r02, r00 );
            r05 = _mm256_add_pd( r03, r01 );
            r06 = _mm256_sub_pd( r02, r00 );
            r07 = _mm256_sub_pd( r03, r01 );
            #endif
            
            _mm256_store_pd( psumN +  8, r04 );
            _mm256_store_pd( psumN + 12, r05 );
            _mm256_store_pd( psumS +  8, r06 );
            _mm256_store_pd( psumS + 12, r07 );
            
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
    const double *pswork1 = swork;
    const double *pswork2 = swork + 64 * n;
    
    double *psumN;
    double *psumS;
    
    // cosine registers
    const __m512d rcosx0 = _mm512_load_pd( cosx +  0 );
    const __m512d rcosx1 = _mm512_load_pd( cosx +  8 );
    const __m512d rcosx2 = _mm512_load_pd( cosx + 16 );
    const __m512d rcosx3 = _mm512_load_pd( cosx + 24 );
    
    // other registers to be used
    __m512d r00, r01, r02, r03,
            r04, r05, r06, r07;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        psumN = sumN + 32 * i3;
        psumS = sumS + 32 * i3;
        
        for ( int i2 = 0; i2 < 2; i2++ ) {
            
            r00 = _mm512_load_pd( pswork1 + 0 );
            r01 = _mm512_load_pd( pswork1 + 8 );
            r02 = _mm512_load_pd( pswork2 + 0 );
            r03 = _mm512_load_pd( pswork2 + 8 );
            
            r04 = _mm512_fmadd_pd( r02, rcosx0, r00 );
            r05 = _mm512_fmadd_pd( r03, rcosx1, r01 );
            r06 = _mm512_fmsub_pd( r02, rcosx0, r00 );
            r07 = _mm512_fmsub_pd( r03, rcosx1, r01 );
            
            _mm512_store_pd( psumN + 0, r04 );
            _mm512_store_pd( psumN + 8, r05 );
            _mm512_store_pd( psumS + 0, r06 );
            _mm512_store_pd( psumS + 8, r07 );
            
            r00 = _mm512_load_pd( pswork1 + 16 );
            r01 = _mm512_load_pd( pswork1 + 24 );
            r02 = _mm512_load_pd( pswork2 + 16 );
            r03 = _mm512_load_pd( pswork2 + 24 );
            
            r04 = _mm512_fmadd_pd( r02, rcosx2, r00 );
            r05 = _mm512_fmadd_pd( r03, rcosx3, r01 );
            r06 = _mm512_fmsub_pd( r02, rcosx2, r00 );
            r07 = _mm512_fmsub_pd( r03, rcosx3, r01 );
            
            _mm512_store_pd( psumN + 16, r04 );
            _mm512_store_pd( psumN + 24, r05 );
            _mm512_store_pd( psumS + 16, r06 );
            _mm512_store_pd( psumS + 24, r07 );
            
            psumN   += 32 * n;
            psumS   += 32 * n;
            pswork1 += 32;
            pswork2 += 32;
            
        }
        
    }
    
}
#endif