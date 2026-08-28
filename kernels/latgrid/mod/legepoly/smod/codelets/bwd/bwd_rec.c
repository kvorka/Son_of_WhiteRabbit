#include <immintrin.h>

extern inline __attribute__((always_inline))
void bwd_rec_c( const int n,
                const int nma,
                const double *restrict fmj,
                const double *restrict cosx2,
                const double *restrict cc,
                      double *restrict pmj1,
                      double *restrict pmj,
                      double *restrict swork )

#if defined ( mem32 )
{
    
    // Memory address of partial sums and coeffs
    const double *pcc = cc;
          double *psw;
    
    // Registers to be used, r00-r03 are reserved
    // for pmj values obtained from reccurrence
    __m256d r00, r01, r02, r03,
            r04, r05, r06, r07,
            r08, r09, r10, r11,
            r12;
    
    // Cycle over weird degree iterator
    for ( int i4 = 0; i4 < nma; i4++ ) {
        
        // Reset the accumulator reference
        psw = swork;
        
        // Legendre polynomial reccurence
        {
            
            r00 = _mm256_load_pd( cosx2 +  0 );
            r01 = _mm256_load_pd( cosx2 +  4 );
            r02 = _mm256_load_pd( cosx2 +  8 );
            r03 = _mm256_load_pd( cosx2 + 12 );
            
            r04 = _mm256_broadcast_sd( fmj + 0 + 3*i4 );
            r05 = _mm256_broadcast_sd( fmj + 1 + 3*i4 );
            
            #if defined ( fma )
            r00 = _mm256_fmsub_pd( r04, r00, r05 );
            r01 = _mm256_fmsub_pd( r04, r01, r05 );
            r02 = _mm256_fmsub_pd( r04, r02, r05 );
            r03 = _mm256_fmsub_pd( r04, r03, r05 );
            #else
            r00 = _mm256_mul_pd( r04, r00 );
            r01 = _mm256_mul_pd( r04, r01 );
            r02 = _mm256_mul_pd( r04, r02 );
            r03 = _mm256_mul_pd( r04, r03 );
            
            r00 = _mm256_sub_pd( r00, r05 );
            r01 = _mm256_sub_pd( r01, r05 );
            r02 = _mm256_sub_pd( r02, r05 );
            r03 = _mm256_sub_pd( r03, r05 );
            #endif
            
            r04 = _mm256_load_pd( pmj1 +  0 );
            r05 = _mm256_load_pd( pmj1 +  4 );
            r06 = _mm256_load_pd( pmj1 +  8 );
            r07 = _mm256_load_pd( pmj1 + 12 );
            
            r08 = _mm256_broadcast_sd( fmj + 2 + 3*i4 );
            
            r04 = _mm256_mul_pd( r08, r04 );
            r05 = _mm256_mul_pd( r08, r05 );
            r06 = _mm256_mul_pd( r08, r06 );
            r07 = _mm256_mul_pd( r08, r07 );
            
            r08 = _mm256_load_pd( pmj +  0 );
            r09 = _mm256_load_pd( pmj +  4 );
            r10 = _mm256_load_pd( pmj +  8 );
            r11 = _mm256_load_pd( pmj + 12 );
            
            _mm256_store_pd( pmj1 +  0, r08 );
            _mm256_store_pd( pmj1 +  4, r09 );
            _mm256_store_pd( pmj1 +  8, r10 );
            _mm256_store_pd( pmj1 + 12, r11 );
            
            #if defined ( fma )
            r00 = _mm256_fmsub_pd( r00, r08, r04 );
            r01 = _mm256_fmsub_pd( r01, r09, r05 );
            r02 = _mm256_fmsub_pd( r02, r10, r06 );
            r03 = _mm256_fmsub_pd( r03, r11, r07 );
            #else
            r00 = _mm256_mul_pd( r00, r08);
            r01 = _mm256_mul_pd( r01, r09);
            r02 = _mm256_mul_pd( r02, r10);
            r03 = _mm256_mul_pd( r03, r11);
            
            r00 = _mm256_sub_pd( r00, r04 );
            r01 = _mm256_sub_pd( r01, r05 );
            r02 = _mm256_sub_pd( r02, r06 );
            r03 = _mm256_sub_pd( r03, r07 );
            #endif
            
            _mm256_store_pd( pmj +  0, r00 );
            _mm256_store_pd( pmj +  4, r01 );
            _mm256_store_pd( pmj +  8, r02 );
            _mm256_store_pd( pmj + 12, r03 );
            
        }
        
        // Loop over number of spectral rows
        for ( int i3 = 0; i3 < n; i3++ ) {
            
            r04 = _mm256_broadcast_sd( pcc + 0 );
            
            r05 = _mm256_load_pd( psw +  0 );
            r06 = _mm256_load_pd( psw +  4 );
            r07 = _mm256_load_pd( psw +  8 );
            r08 = _mm256_load_pd( psw + 12 );
            
            #if defined ( fma )
            r05 = _mm256_fmadd_pd( r00, r04, r05 );
            r06 = _mm256_fmadd_pd( r01, r04, r06 );
            r07 = _mm256_fmadd_pd( r02, r04, r07 );
            r08 = _mm256_fmadd_pd( r03, r04, r08 );
            #else
            r09 = _mm256_mul_pd( r00, r04 );
            r10 = _mm256_mul_pd( r01, r04 );
            r11 = _mm256_mul_pd( r02, r04 );
            r12 = _mm256_mul_pd( r03, r04 );
            
            r05 = _mm256_add_pd( r09, r05 );
            r06 = _mm256_add_pd( r10, r06 );
            r07 = _mm256_add_pd( r11, r07 );
            r08 = _mm256_add_pd( r12, r08 );
            #endif
            
            _mm256_store_pd( psw +  0, r05 );
            _mm256_store_pd( psw +  4, r06 );
            _mm256_store_pd( psw +  8, r07 );
            _mm256_store_pd( psw + 12, r08 );
            
            r04 = _mm256_broadcast_sd( pcc + 1 );
            
            r05 = _mm256_load_pd( psw + 16 );
            r06 = _mm256_load_pd( psw + 20 );
            r07 = _mm256_load_pd( psw + 24 );
            r08 = _mm256_load_pd( psw + 28 );
            
            #if defined ( fma )
            r05 = _mm256_fmadd_pd( r00, r04, r05 );
            r06 = _mm256_fmadd_pd( r01, r04, r06 );
            r07 = _mm256_fmadd_pd( r02, r04, r07 );
            r08 = _mm256_fmadd_pd( r03, r04, r08 );
            #else
            r09 = _mm256_mul_pd( r00, r04 );
            r10 = _mm256_mul_pd( r01, r04 );
            r11 = _mm256_mul_pd( r02, r04 );
            r12 = _mm256_mul_pd( r03, r04 );
            
            r05 = _mm256_add_pd( r09, r05 );
            r06 = _mm256_add_pd( r10, r06 );
            r07 = _mm256_add_pd( r11, r07 );
            r08 = _mm256_add_pd( r12, r08 );
            #endif
            
            _mm256_store_pd( psw + 16, r05 );
            _mm256_store_pd( psw + 20, r06 );
            _mm256_store_pd( psw + 24, r07 );
            _mm256_store_pd( psw + 28, r08 );
            
            r04 = _mm256_broadcast_sd( pcc + 2 );
            
            r05 = _mm256_load_pd( psw + 32 );
            r06 = _mm256_load_pd( psw + 36 );
            r07 = _mm256_load_pd( psw + 40 );
            r08 = _mm256_load_pd( psw + 44 );
            
            #if defined ( fma )
            r05 = _mm256_fmadd_pd( r00, r04, r05 );
            r06 = _mm256_fmadd_pd( r01, r04, r06 );
            r07 = _mm256_fmadd_pd( r02, r04, r07 );
            r08 = _mm256_fmadd_pd( r03, r04, r08 );
            #else
            r09 = _mm256_mul_pd( r00, r04 );
            r10 = _mm256_mul_pd( r01, r04 );
            r11 = _mm256_mul_pd( r02, r04 );
            r12 = _mm256_mul_pd( r03, r04 );
            
            r05 = _mm256_add_pd( r09, r05 );
            r06 = _mm256_add_pd( r10, r06 );
            r07 = _mm256_add_pd( r11, r07 );
            r08 = _mm256_add_pd( r12, r08 );
            #endif
            
            _mm256_store_pd( psw + 32, r05 );
            _mm256_store_pd( psw + 36, r06 );
            _mm256_store_pd( psw + 40, r07 );
            _mm256_store_pd( psw + 44, r08 );
            
            r04 = _mm256_broadcast_sd( pcc + 3 );
            
            r05 = _mm256_load_pd( psw + 48 );
            r06 = _mm256_load_pd( psw + 52 );
            r07 = _mm256_load_pd( psw + 56 );
            r08 = _mm256_load_pd( psw + 60 );
            
            #if defined ( fma )
            r05 = _mm256_fmadd_pd( r00, r04, r05 );
            r06 = _mm256_fmadd_pd( r01, r04, r06 );
            r07 = _mm256_fmadd_pd( r02, r04, r07 );
            r08 = _mm256_fmadd_pd( r03, r04, r08 );
            #else
            r09 = _mm256_mul_pd( r00, r04 );
            r10 = _mm256_mul_pd( r01, r04 );
            r11 = _mm256_mul_pd( r02, r04 );
            r12 = _mm256_mul_pd( r03, r04 );
            
            r05 = _mm256_add_pd( r09, r05 );
            r06 = _mm256_add_pd( r10, r06 );
            r07 = _mm256_add_pd( r11, r07 );
            r08 = _mm256_add_pd( r12, r08 );
            #endif
            
            _mm256_store_pd( psw + 48, r05 );
            _mm256_store_pd( psw + 52, r06 );
            _mm256_store_pd( psw + 56, r07 );
            _mm256_store_pd( psw + 60, r08 );
            
            pcc +=  4;
            psw += 64;
            
        }
        
    }
    
}
#else
{
    
    // Memory address of partial sums and coeffs
    const double *pcc = cc;
          double *psw;
    
    // Registers to be used, r00-r03 are reserved
    // for pmj values obtained from reccurrence
    __m512d r00, r01, r02, r03,
            r04, r05, r06, r07,
            r08, r09, r10, r11;
    
    // Cycle over weird degree iterator
    for ( int i4 = 0; i4 < nma; i4++ ) {
        
        // Reset the accumulator reference
        psw = swork;
        
        // Legendre polynomial reccurence
        {
            
            r00 = _mm512_load_pd( cosx2 +  0 );
            r01 = _mm512_load_pd( cosx2 +  8 );
            r02 = _mm512_load_pd( cosx2 + 16 );
            r03 = _mm512_load_pd( cosx2 + 24 );
            
            r04 = _mm512_set1_pd( *( fmj + 0 + 3*i4 ) );
            r05 = _mm512_set1_pd( *( fmj + 1 + 3*i4 ) );
            
            r00 = _mm512_fmsub_pd( r04, r00, r05 );
            r01 = _mm512_fmsub_pd( r04, r01, r05 );
            r02 = _mm512_fmsub_pd( r04, r02, r05 );
            r03 = _mm512_fmsub_pd( r04, r03, r05 );
            
            r04 = _mm512_load_pd( pmj1 +  0 );
            r05 = _mm512_load_pd( pmj1 +  8 );
            r06 = _mm512_load_pd( pmj1 + 16 );
            r07 = _mm512_load_pd( pmj1 + 24 );
            
            r08 = _mm512_set1_pd( *( fmj + 2 + 3*i4 ) );
            
            r04 = _mm512_mul_pd( r08, r04 );
            r05 = _mm512_mul_pd( r08, r05 );
            r06 = _mm512_mul_pd( r08, r06 );
            r07 = _mm512_mul_pd( r08, r07 );
            
            r08 = _mm512_load_pd( pmj +  0 );
            r09 = _mm512_load_pd( pmj +  8 );
            r10 = _mm512_load_pd( pmj + 16 );
            r11 = _mm512_load_pd( pmj + 24 );
            
            _mm512_store_pd( pmj1 +  0, r08 );
            _mm512_store_pd( pmj1 +  8, r09 );
            _mm512_store_pd( pmj1 + 16, r10 );
            _mm512_store_pd( pmj1 + 24, r11 );
            
            r00 = _mm512_fmsub_pd( r00, r08, r04 );
            r01 = _mm512_fmsub_pd( r01, r09, r05 );
            r02 = _mm512_fmsub_pd( r02, r10, r06 );
            r03 = _mm512_fmsub_pd( r03, r11, r07 );
            
            _mm512_store_pd( pmj +  0, r00 );
            _mm512_store_pd( pmj +  8, r01 );
            _mm512_store_pd( pmj + 16, r02 );
            _mm512_store_pd( pmj + 24, r03 );
            
        }
        
        // Loop over number of spectral rows
        for ( int i3 = 0; i3 < n; i3++ ) {
            
            r04 = _mm512_set1_pd( *( pcc + 0 ) );
            
            r05 = _mm512_load_pd( psw +  0 );
            r06 = _mm512_load_pd( psw +  8 );
            r07 = _mm512_load_pd( psw + 16 );
            r08 = _mm512_load_pd( psw + 24 );
            
            r05 = _mm512_fmadd_pd( r00, r04, r05 );
            r06 = _mm512_fmadd_pd( r01, r04, r06 );
            r07 = _mm512_fmadd_pd( r02, r04, r07 );
            r08 = _mm512_fmadd_pd( r03, r04, r08 );
            
            _mm512_store_pd( psw +  0, r05 );
            _mm512_store_pd( psw +  8, r06 );
            _mm512_store_pd( psw + 16, r07 );
            _mm512_store_pd( psw + 24, r08 );
            
            r04 = _mm512_set1_pd( *( pcc + 1 ) );
            
            r05 = _mm512_load_pd( psw + 32 );
            r06 = _mm512_load_pd( psw + 40 );
            r07 = _mm512_load_pd( psw + 48 );
            r08 = _mm512_load_pd( psw + 56 );
            
            r05 = _mm512_fmadd_pd( r00, r04, r05 );
            r06 = _mm512_fmadd_pd( r01, r04, r06 );
            r07 = _mm512_fmadd_pd( r02, r04, r07 );
            r08 = _mm512_fmadd_pd( r03, r04, r08 );
            
            _mm512_store_pd( psw + 32, r05 );
            _mm512_store_pd( psw + 40, r06 );
            _mm512_store_pd( psw + 48, r07 );
            _mm512_store_pd( psw + 56, r08 );
            
            r04 = _mm512_set1_pd( *( pcc + 2 ) );
            
            r05 = _mm512_load_pd( psw + 64 );
            r06 = _mm512_load_pd( psw + 72 );
            r07 = _mm512_load_pd( psw + 80 );
            r08 = _mm512_load_pd( psw + 88 );
            
            r05 = _mm512_fmadd_pd( r00, r04, r05 );
            r06 = _mm512_fmadd_pd( r01, r04, r06 );
            r07 = _mm512_fmadd_pd( r02, r04, r07 );
            r08 = _mm512_fmadd_pd( r03, r04, r08 );
            
            _mm512_store_pd( psw + 64, r05 );
            _mm512_store_pd( psw + 72, r06 );
            _mm512_store_pd( psw + 80, r07 );
            _mm512_store_pd( psw + 88, r08 );
            
            r04 = _mm512_set1_pd( *( pcc + 3 ) );
            
            r05 = _mm512_load_pd( psw +  96 );
            r06 = _mm512_load_pd( psw + 104 );
            r07 = _mm512_load_pd( psw + 112 );
            r08 = _mm512_load_pd( psw + 120 );
            
            r05 = _mm512_fmadd_pd( r00, r04, r05 );
            r06 = _mm512_fmadd_pd( r01, r04, r06 );
            r07 = _mm512_fmadd_pd( r02, r04, r07 );
            r08 = _mm512_fmadd_pd( r03, r04, r08 );
            
            _mm512_store_pd( psw +  96, r05 );
            _mm512_store_pd( psw + 104, r06 );
            _mm512_store_pd( psw + 112, r07 );
            _mm512_store_pd( psw + 120, r08 );
            
            pcc +=   4;
            psw += 128;
            
        }
        
    }
    
}
#endif