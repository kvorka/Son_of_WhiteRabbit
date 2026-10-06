#include "../codelets/bwd_set.h"
#include "../codelets/bwd_rec.h"
#include "../codelets/bwd_rsc.h"

extern inline __attribute__((always_inline))
void bwd_m_c( const int n,
              const int m,
              const int nma,
              const double *restrict fmj,
              const double *restrict cosx,
              const double *restrict sinx,
              const double *restrict cosx2,
              const double *restrict cc,
                    double *restrict pmm,
                    double *restrict pmj,
                    double *restrict pmj1,
                    double *restrict swork,
                    double *restrict sumN,
                    double *restrict sumS )

{
    
    bwd_set_c( n, m, fmj, cosx, sinx, cc, pmm, pmj1, pmj, swork );
    bwd_rec_c( n, nma, fmj+3, cosx2, cc+4*n, pmj1, pmj, swork );
    bwd_rsc_c( n, cosx, swork, sumN, sumS );
    
}

extern inline __attribute__((always_inline))
void bwd_end_c( const int n,
                const int m,
                const double *restrict fmj,
                const double *restrict cosx,
                const double *restrict sinx,
                const double *restrict cc,
                      double *restrict pmm,
                      double *restrict pmj,
                      double *restrict pmj1,
                      double *restrict swork,
                      double *restrict sumN,
                      double *restrict sumS )

{
    
    bwd_set_c( n, m, fmj, cosx, sinx, cc, pmm, pmj1, pmj, swork );
    bwd_rsc_c( n, cosx, swork, sumN, sumS );
    
}