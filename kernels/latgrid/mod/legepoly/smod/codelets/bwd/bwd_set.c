#include <immintrin.h>

extern inline __attribute__((always_inline))
void bwd_set_c( const int n,
                const int ma1,
                const double *restrict cff,
                const double *restrict cosx,
                const double *restrict sinx,
                const double *restrict cc,
                      double *restrict pmm,
                      double *restrict pmj1,
                      double *restrict pmj,
                      double *restrict swork )

#if defined ( mem32 )
{   
    
    // Registers to be used
    __m256d r00, r01, r02, r03,
            r04, r05, r06, r07,
            r08;
    
    // pmm reccurence
    {
        
        r04 = _mm256_broadcast_sd( cff );
        
        if ( ma1 == 1 ) {
            
            r00 = r04;
            r01 = r04;
            r02 = r04;
            r03 = r04;
            
        } else {
            
            r05 = _mm256_load_pd( sinx +  0 );
            r06 = _mm256_load_pd( sinx +  4 );
            r07 = _mm256_load_pd( sinx +  8 );
            r08 = _mm256_load_pd( sinx + 12 );
            
            r05 = _mm256_mul_pd( r04, r05 );
            r06 = _mm256_mul_pd( r04, r06 );
            r07 = _mm256_mul_pd( r04, r07 );
            r08 = _mm256_mul_pd( r04, r08 );
            
            r00 = _mm256_load_pd( pmm +  0 );
            r01 = _mm256_load_pd( pmm +  4 );
            r02 = _mm256_load_pd( pmm +  8 );
            r03 = _mm256_load_pd( pmm + 12 );
            
            r00 = _mm256_mul_pd( r05, r00 );
            r01 = _mm256_mul_pd( r06, r01 );
            r02 = _mm256_mul_pd( r07, r02 );
            r03 = _mm256_mul_pd( r08, r03 );
            
        }
        
        _mm256_store_pd( pmm +  0, r00 );
        _mm256_store_pd( pmm +  4, r01 );
        _mm256_store_pd( pmm +  8, r02 );
        _mm256_store_pd( pmm + 12, r03 );
        
    }
    
    // pmj1 and pmj set-up
    {
        
        r08 = _mm256_setzero_pd();
        
        _mm256_store_pd( pmj1 +  0, r08 );
        _mm256_store_pd( pmj1 +  4, r08 );
        _mm256_store_pd( pmj1 +  8, r08 );
        _mm256_store_pd( pmj1 + 12, r08 );
        
        r04 = _mm256_load_pd( cosx +  0 );
        r05 = _mm256_load_pd( cosx +  4 );
        r06 = _mm256_load_pd( cosx +  8 );
        r07 = _mm256_load_pd( cosx + 12 );
        
        r00 = _mm256_div_pd( r00, r04 );
        r01 = _mm256_div_pd( r01, r05 );
        r02 = _mm256_div_pd( r02, r06 );
        r03 = _mm256_div_pd( r03, r07 );
        
        _mm256_store_pd( pmj +  0, r00 );
        _mm256_store_pd( pmj +  4, r01 );
        _mm256_store_pd( pmj +  8, r02 );
        _mm256_store_pd( pmj + 12, r03 );
        
    }
    
    // Memory address of partial sums and coeffs
    const double *pcc = cc;
          double *psw = swork;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        r04 = _mm256_broadcast_sd( pcc + 0 );
        
        r05 = _mm256_mul_pd( r00, r04 );
        r06 = _mm256_mul_pd( r01, r04 );
        r07 = _mm256_mul_pd( r02, r04 );
        r08 = _mm256_mul_pd( r03, r04 );
        
        _mm256_store_pd( psw +  0, r05 );
        _mm256_store_pd( psw +  4, r06 );
        _mm256_store_pd( psw +  8, r07 );
        _mm256_store_pd( psw + 12, r08 );
        
        r04 = _mm256_broadcast_sd( pcc + 1 );
        
        r05 = _mm256_mul_pd( r00, r04 );
        r06 = _mm256_mul_pd( r01, r04 );
        r07 = _mm256_mul_pd( r02, r04 );
        r08 = _mm256_mul_pd( r03, r04 );
        
        _mm256_store_pd( psw + 16, r05 );
        _mm256_store_pd( psw + 20, r06 );
        _mm256_store_pd( psw + 24, r07 );
        _mm256_store_pd( psw + 28, r08 );
        
        r04 = _mm256_broadcast_sd( pcc + 2 );
        
        r05 = _mm256_mul_pd( r00, r04 );
        r06 = _mm256_mul_pd( r01, r04 );
        r07 = _mm256_mul_pd( r02, r04 );
        r08 = _mm256_mul_pd( r03, r04 );
        
        _mm256_store_pd( psw + 32, r05 );
        _mm256_store_pd( psw + 36, r06 );
        _mm256_store_pd( psw + 40, r07 );
        _mm256_store_pd( psw + 44, r08 );
        
        r04 = _mm256_broadcast_sd( pcc + 3 );
        
        r05 = _mm256_mul_pd( r00, r04 );
        r06 = _mm256_mul_pd( r01, r04 );
        r07 = _mm256_mul_pd( r02, r04 );
        r08 = _mm256_mul_pd( r03, r04 );
        
        _mm256_store_pd( psw + 48, r05 );
        _mm256_store_pd( psw + 52, r06 );
        _mm256_store_pd( psw + 56, r07 );
        _mm256_store_pd( psw + 60, r08 );
        
        pcc +=  4;
        psw += 64;
        
    }
    
}
#else
{   
    
    // Registers to be used
    __m512d r00, r01, r02, r03,
            r04, r05, r06, r07,
            r08;
    
    // pmm reccurence
    {
        
        r04 = _mm512_set1_pd( *cff );
        
        if ( ma1 == 1 ) {
            
            r00 = r04;
            r01 = r04;
            r02 = r04;
            r03 = r04;
            
        } else {
            
            r05 = _mm512_load_pd( sinx +  0 );
            r06 = _mm512_load_pd( sinx +  8 );
            r07 = _mm512_load_pd( sinx + 16 );
            r08 = _mm512_load_pd( sinx + 24 );
            
            r05 = _mm512_mul_pd( r04, r05 );
            r06 = _mm512_mul_pd( r04, r06 );
            r07 = _mm512_mul_pd( r04, r07 );
            r08 = _mm512_mul_pd( r04, r08 );
            
            r00 = _mm512_load_pd( pmm +  0 );
            r01 = _mm512_load_pd( pmm +  8 );
            r02 = _mm512_load_pd( pmm + 16 );
            r03 = _mm512_load_pd( pmm + 24 );
            
            r00 = _mm512_mul_pd( r05, r00 );
            r01 = _mm512_mul_pd( r06, r01 );
            r02 = _mm512_mul_pd( r07, r02 );
            r03 = _mm512_mul_pd( r08, r03 );
            
        }
        
        _mm512_store_pd( pmm +  0, r00 );
        _mm512_store_pd( pmm +  8, r01 );
        _mm512_store_pd( pmm + 16, r02 );
        _mm512_store_pd( pmm + 24, r03 );
        
    }
    
    // pmj1 and pmj set-up
    {
        
        r08 = _mm512_setzero_pd();
        
        _mm512_store_pd( pmj1 +  0, r08 );
        _mm512_store_pd( pmj1 +  8, r08 );
        _mm512_store_pd( pmj1 + 16, r08 );
        _mm512_store_pd( pmj1 + 24, r08 );
        
        r04 = _mm512_load_pd( cosx +  0 );
        r05 = _mm512_load_pd( cosx +  8 );
        r06 = _mm512_load_pd( cosx + 16 );
        r07 = _mm512_load_pd( cosx + 24 );
        
        r00 = _mm512_div_pd( r00, r04 );
        r01 = _mm512_div_pd( r01, r05 );
        r02 = _mm512_div_pd( r02, r06 );
        r03 = _mm512_div_pd( r03, r07 );
        
        _mm512_store_pd( pmj +  0, r00 );
        _mm512_store_pd( pmj +  8, r01 );
        _mm512_store_pd( pmj + 16, r02 );
        _mm512_store_pd( pmj + 24, r03 );
        
    }
    
    // Memory address of partial sums and coeffs
    const double *pcc = cc;
          double *psw = swork;
    
    for ( int i3 = 0; i3 < n; i3++ ) {
        
        r04 = _mm512_set1_pd( *( pcc + 0 ) );
        
        r05 = _mm512_mul_pd( r00, r04 );
        r06 = _mm512_mul_pd( r01, r04 );
        r07 = _mm512_mul_pd( r02, r04 );
        r08 = _mm512_mul_pd( r03, r04 );
        
        _mm512_store_pd( psw +  0, r05 );
        _mm512_store_pd( psw +  8, r06 );
        _mm512_store_pd( psw + 16, r07 );
        _mm512_store_pd( psw + 24, r08 );
        
        r04 = _mm512_set1_pd( *( pcc + 1 ) );
        
        r05 = _mm512_mul_pd( r00, r04 );
        r06 = _mm512_mul_pd( r01, r04 );
        r07 = _mm512_mul_pd( r02, r04 );
        r08 = _mm512_mul_pd( r03, r04 );
        
        _mm512_store_pd( psw + 32, r05 );
        _mm512_store_pd( psw + 40, r06 );
        _mm512_store_pd( psw + 48, r07 );
        _mm512_store_pd( psw + 56, r08 );
        
        r04 = _mm512_set1_pd( *( pcc + 2 ) );
        
        r05 = _mm512_mul_pd( r00, r04 );
        r06 = _mm512_mul_pd( r01, r04 );
        r07 = _mm512_mul_pd( r02, r04 );
        r08 = _mm512_mul_pd( r03, r04 );
        
        _mm512_store_pd( psw + 64, r05 );
        _mm512_store_pd( psw + 72, r06 );
        _mm512_store_pd( psw + 80, r07 );
        _mm512_store_pd( psw + 88, r08 );
        
        r04 = _mm512_set1_pd( *( pcc + 3 ) );
        
        r05 = _mm512_mul_pd( r00, r04 );
        r06 = _mm512_mul_pd( r01, r04 );
        r07 = _mm512_mul_pd( r02, r04 );
        r08 = _mm512_mul_pd( r03, r04 );
        
        _mm512_store_pd( psw +  96, r05 );
        _mm512_store_pd( psw + 104, r06 );
        _mm512_store_pd( psw + 112, r07 );
        _mm512_store_pd( psw + 120, r08 );
        
        pcc +=   4;
        psw += 128;
        
    }
    
}
#endif