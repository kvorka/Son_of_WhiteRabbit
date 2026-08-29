#include <stddef.h>
#include <complex.h>
#include <immintrin.h>
#include <emmintrin.h>

extern inline __attribute__((always_inline))
void copy_vgradT_vcurlv_c( const int n,
                        const double complex *restrict v,
                        const double complex *restrict q,
                        const double complex *restrict curlv,
                              double complex *restrict ca )
{
    
    // Casting memory addresses
    const double *pv  = ( const double * ) v;
    const double *pq  = ( const double * ) q;
    const double *pcv = ( const double * ) curlv;
          double *pca = (       double * ) ca;
    
    // Memory addresses to be used
    const double *pv1 = pv + 0*n;
    const double *pv2 = pv + 2*n;
    const double *pv3 = pv + 4*n;
    
    const double *pq1 = pq + 0*n;
    const double *pq2 = pq + 2*n;
    const double *pq3 = pq + 4*n;

    const double *pcv1 = pcv + 0*n;
    const double *pcv2 = pcv + 2*n;
    const double *pcv3 = pcv + 4*n;
    
    // Iterator
    int i = 0;
    
    // Body of the cycle
    {
        
        // Registers to be used
        __m128d s00, s01;
        __m256d r00, r01, r02, r03, 
                r04, r05, r06, r07;
        
        // Main cycle
        for ( ; i <= n-2; i += 2 ) {
            
            r00 = _mm256_loadu_pd( pv1 );
            r01 = _mm256_loadu_pd( pq1 );
            r04 = _mm256_loadu_pd( pcv1 );
            r05 = _mm256_loadu_pd( pv2  );
            
            r02 = _mm256_permute2f128_pd( r00, r01, 0x20 );
            r03 = _mm256_permute2f128_pd( r00, r01, 0x31 );
            r06 = _mm256_permute2f128_pd( r04, r05, 0x20 );
            r07 = _mm256_permute2f128_pd( r04, r05, 0x31 );
            
            _mm256_storeu_pd( pca +  0, r02 );
            _mm256_storeu_pd( pca + 18, r03 );
            _mm256_storeu_pd( pca +  4, r06 );
            _mm256_storeu_pd( pca + 22, r07 );
            
            r00 = _mm256_loadu_pd( pq2  );
            r01 = _mm256_loadu_pd( pcv2 );
            r04 = _mm256_loadu_pd( pv3 );
            r05 = _mm256_loadu_pd( pq3 );
            
            r02 = _mm256_permute2f128_pd( r00, r01, 0x20 );
            r03 = _mm256_permute2f128_pd( r00, r01, 0x31 );
            r06 = _mm256_permute2f128_pd( r04, r05, 0x20 );
            r07 = _mm256_permute2f128_pd( r04, r05, 0x31 );
            
            _mm256_storeu_pd( pca +  8, r02 );
            _mm256_storeu_pd( pca + 26, r03 );
            _mm256_storeu_pd( pca + 12, r06 );
            _mm256_storeu_pd( pca + 30, r07 );
            
            s00 = _mm_loadu_pd( pcv3 + 0 );
            s01 = _mm_loadu_pd( pcv3 + 2 );
            
            _mm_storeu_pd( pca + 16, s00 );
            _mm_storeu_pd( pca + 34, s01 );
            
            pv1 += 4;
            pv2 += 4;
            pv3 += 4;
            
            pq1 += 4;
            pq2 += 4;
            pq3 += 4;
            
            pcv1 += 4;
            pcv2 += 4;
            pcv3 += 4;
            
            pca += 36;
            
        }
        
    }
    
    // SSE remainder
    if ( i < n ) {
        
         // Registers to be used
        __m128d s00, s01, s02;
        
        // Non-cycle remainder
        s00 = _mm_loadu_pd( pv1  );
        s01 = _mm_loadu_pd( pq1  );
        s02 = _mm_loadu_pd( pcv1 );
        
        _mm_storeu_pd( pca + 0, s00 );
        _mm_storeu_pd( pca + 2, s01 );
        _mm_storeu_pd( pca + 4, s02 );

        s00 = _mm_loadu_pd( pv2  );
        s01 = _mm_loadu_pd( pq2  );
        s02 = _mm_loadu_pd( pcv2 );
        
        _mm_storeu_pd( pca +  6, s00 );
        _mm_storeu_pd( pca +  8, s01 );
        _mm_storeu_pd( pca + 10, s02 );
        
        s00 = _mm_loadu_pd( pv3  );
        s01 = _mm_loadu_pd( pq3  );
        s02 = _mm_loadu_pd( pcv3 );
        
        _mm_storeu_pd( pca + 12, s00 );
        _mm_storeu_pd( pca + 14, s01 );
        _mm_storeu_pd( pca + 16, s02 );
        
    }
    
}