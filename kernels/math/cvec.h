#pragma once

/*********************************************************
    This part is going to be read by fortran prepro-
    cessor, therefore no indentation to make it pretty.
**********************************************************/
#if defined (__AVX512F__)
#define vlen 8
#define alignement 64
#else
#define vlen 4
#define alignement 32
#endif

/*********************************************************
    This part is solely for C part of the code.
**********************************************************/
#if !defined( __GFORTRAN__ )
    
    #include <stddef.h>
    #include <complex.h>
    #include <immintrin.h>
    #include <emmintrin.h>
    
    #if vlen == 4
        
        typedef __m256d __td;
        
        #define cmask 0x05
        
        #define _t_set1_pd(val)        _mm256_set1_pd(val)
        #define _t_setzero_pd()        _mm256_setzero_pd()
        #define _t_setalts_pd()        _mm256_set_pd(-0., 0.,-0., 0.)
        #define _t_setaltz_pd()        _mm256_set_pd( 0.,-0., 0.,-0.)
        #define _t_load_pd(ptr)        _mm256_load_pd(ptr)
        #define _t_loadu_pd(ptr)       _mm256_loadu_pd(ptr)
        #define _t_store_pd(ptr,val)   _mm256_store_pd((ptr),(val))
        #define _t_storeu_pd(ptr,val)  _mm256_storeu_pd((ptr),(val))
        #define _t_add_pd(a, b)        _mm256_add_pd((a),(b))
        #define _t_sub_pd(a, b)        _mm256_sub_pd((a),(b))
        #define _t_mul_pd(a, b)        _mm256_mul_pd((a),(b))
        #define _t_div_pd(a, b)        _mm256_div_pd((a), (b))
        #define _t_unpacklo_pd(a, b)   _mm256_unpacklo_pd((a), (b))
        #define _t_unpackhi_pd(a, b)   _mm256_unpackhi_pd((a), (b))
        #define _t_permute_pd(a, mask) _mm256_permute_pd((a), (mask))
        #define _t_xor_pd(a, sign)     _mm256_xor_pd((a), (sign))
        
        #if defined (__FMA__)
            #define _t_fmadd_pd(a, b, c)  _mm256_fmadd_pd((a),(b),(c))
            #define _t_fmsub_pd(a, b, c)  _mm256_fmsub_pd((a),(b),(c))
            #define _t_fnmadd_pd(a, b, c) _mm256_fnmadd_pd((a),(b),(c))
            #define _t_fnmsub_pd(a, b, c) _mm256_fnmsub_pd((a),(b),(c))
        #endif
        
    #else
        
        typedef __m512d __td;
        
        #define cmask 0x55
        
        #define _t_set1_pd(val)        _mm512_set1_pd(val)
        #define _t_setzero_pd()        _mm512_setzero_pd()
        #define _t_setalts_pd()        _mm512_set_pd(-0., 0.,-0., 0.,-0.,  0.,-0., 0.)
        #define _t_setaltz_pd()        _mm512_set_pd( 0.,-0., 0.,-0., 0., -0., 0.,-0.)
        #define _t_load_pd(ptr)        _mm512_load_pd(ptr)
        #define _t_loadu_pd(ptr)       _mm512_loadu_pd(ptr)
        #define _t_store_pd(ptr,val)   _mm512_store_pd((ptr),(val))
        #define _t_storeu_pd(ptr,val)  _mm512_storeu_pd((ptr),(val))
        #define _t_add_pd(a, b)        _mm512_add_pd((a),(b))
        #define _t_sub_pd(a, b)        _mm512_sub_pd((a),(b))
        #define _t_mul_pd(a, b)        _mm512_mul_pd((a),(b))
        #define _t_div_pd(a, b)        _mm512_div_pd((a), (b))
        #define _t_unpacklo_pd(a, b)   _mm512_unpacklo_pd((a), (b))
        #define _t_unpackhi_pd(a, b)   _mm512_unpackhi_pd((a), (b))
        #define _t_permute_pd(a, mask) _mm512_permute_pd((a), (mask))
        #define _t_xor_pd(a, sign)     _mm512_xor_pd((a), (sign))
        
        #define _t_fmadd_pd(a, b, c)  _mm512_fmadd_pd((a),(b),(c))
        #define _t_fmsub_pd(a, b, c)  _mm512_fmsub_pd((a),(b),(c))
        #define _t_fnmadd_pd(a, b, c) _mm512_fnmadd_pd((a),(b),(c))
        #define _t_fnmsub_pd(a, b, c) _mm512_fnmsub_pd((a),(b),(c))
        
    #endif
#endif