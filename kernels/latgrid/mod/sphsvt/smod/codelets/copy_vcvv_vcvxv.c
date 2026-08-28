#include <stddef.h>
#include <complex.h>
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
    
    // Registers to be used
    __m128d rv1,  rv2,  rv3,
            rq1,  rq2,  rq3,
            rcv1, rcv2, rcv3;
    
    // Main cycle
    for ( int i = 0; i < n; i++ ) {
        
        rv1  = _mm_loadu_pd( pv1  );
        rq1  = _mm_loadu_pd( pq1  );
        rcv1 = _mm_loadu_pd( pcv1 );

        rv2  = _mm_loadu_pd( pv2  );
        rq2  = _mm_loadu_pd( pq2  );
        rcv2 = _mm_loadu_pd( pcv2 );

        rv3  = _mm_loadu_pd( pv3  );
        rq3  = _mm_loadu_pd( pq3  );
        rcv3 = _mm_loadu_pd( pcv3 );
        
        _mm_storeu_pd( pca +  0, rv1  );
        _mm_storeu_pd( pca +  2, rq1  );
        _mm_storeu_pd( pca +  4, rcv1 );
        
        _mm_storeu_pd( pca +  6, rv2  );
        _mm_storeu_pd( pca +  8, rq2  );
        _mm_storeu_pd( pca + 10, rcv2 );
        
        _mm_storeu_pd( pca + 12, rv3  );
        _mm_storeu_pd( pca + 14, rq3  );
        _mm_storeu_pd( pca + 16, rcv3 );
        
        pv1 += 2;
        pv2 += 2;
        pv3 += 2;
        
        pq1 += 2;
        pq2 += 2;
        pq3 += 2;
        
        pcv1 += 2;
        pcv2 += 2;
        pcv3 += 2;
        
        pca += 18;
        
    }
    
}