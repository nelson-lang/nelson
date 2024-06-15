//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include <cstring>
#include <fftw3.h>
#include "FFTWWrapper.hpp"
#include "FftHelpers.hpp"
#include "characters_encoding.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
#define DEFAULT_FFT_METHOD FftPlannerMethod::ESTIMATE
static FftPlannerMethod currentFftMethod = DEFAULT_FFT_METHOD;
//=============================================================================
bool asForwardDouble = true;
bool asForwardSingle = true;
//=============================================================================
static fftw_complex* in = nullptr;
static fftw_complex* out = nullptr;
static fftw_plan p_forward = nullptr;
static fftw_plan p_backward = nullptr;
static int zN = 0;
//=============================================================================
static fftwf_complex* inf = nullptr;
static fftwf_complex* outf = nullptr;
static fftwf_plan pf_forward = nullptr;
static fftwf_plan pf_backward = nullptr;
static int cN = 0;
//=============================================================================
static size_t sizeOfDouble = sizeof(double);
static size_t sizeOfSingle = sizeof(single);
//=============================================================================
static int
getFftFlags(FftPlannerMethod /*unused*/, int Narg)
{
    int flags;
    switch (currentFftMethod) {
    case MEASURE: {
        flags = FFTW_MEASURE;
    } break;
    case PATIENT: {
        flags = FFTW_PATIENT;
    } break;
    case EXHAUSTIVE: {
        flags = FFTW_EXHAUSTIVE;
    } break;
    case HYBRID: {
        if (Narg <= 8192) {
            flags = FFTW_MEASURE;
        } else {
            flags = FFTW_ESTIMATE;
        }
    } break;
    case ESTIMATE:
    default: {
        flags = FFTW_ESTIMATE;
    } break;
    }
    return flags;
}
//=============================================================================
bool
setDoubleWisdomInformation(const std::wstring& info)
{
    std::string uinfo = wstring_to_utf8(info);
    return (dyn_fftw_import_wisdom_from_string(uinfo.c_str()) == 1);
}
//=============================================================================
bool
setSingleWisdomInformation(const std::wstring& info)
{
    std::string uinfo = wstring_to_utf8(info);
    return (dyn_fftwf_import_wisdom_from_string(uinfo.c_str()) == 1);
}
//=============================================================================
bool
setPlannerInformation(FftPlannerMethod newMethod)
{
    bool res = true;
    switch (newMethod) {
    case FftPlannerMethod::ESTIMATE: {
        currentFftMethod = FftPlannerMethod::ESTIMATE;
    } break;
    case FftPlannerMethod::EXHAUSTIVE: {
        currentFftMethod = FftPlannerMethod::EXHAUSTIVE;
    } break;
    case FftPlannerMethod::HYBRID: {
        currentFftMethod = FftPlannerMethod::HYBRID;
    } break;
    case FftPlannerMethod::MEASURE: {
        currentFftMethod = FftPlannerMethod::MEASURE;
    } break;
    case FftPlannerMethod::PATIENT: {
        currentFftMethod = FftPlannerMethod::PATIENT;
    } break;
    case FftPlannerMethod::UNKNOWN:
    default: {
        currentFftMethod = DEFAULT_FFT_METHOD;
        res = false;
    } break;
    }
    return res;
}
//=============================================================================
void
resetDoubleWisdom()
{
    dyn_fftw_forget_wisdom();
}
//=============================================================================
void
resetSingleWisdom()
{
    dyn_fftwf_forget_wisdom();
}
//=============================================================================
void
resetPlanner()
{
    setPlannerInformation(DEFAULT_FFT_METHOD);
}
//=============================================================================
static void
dcomplex_fft_init(int Narg, bool forward)
{
    if (zN == Narg && asForwardDouble == forward) {
        return;
    }
    if (zN != 0) {
        if (p_forward != nullptr) {
            dyn_fftw_destroy_plan(p_forward);
            p_forward = nullptr;
        }
        if (p_backward != nullptr) {
            dyn_fftw_destroy_plan(p_backward);
            p_backward = nullptr;
        }
        dyn_fftw_free(in);
        dyn_fftw_free(out);
    }
    in = (fftw_complex*)dyn_fftw_malloc(sizeof(fftw_complex) * (size_t)Narg);
    out = (fftw_complex*)dyn_fftw_malloc(sizeof(fftw_complex) * (size_t)Narg);
    int flags = getFftFlags(currentFftMethod, Narg);
    if (forward) {
        p_forward = dyn_fftw_plan_dft_1d(Narg, in, out, FFTW_FORWARD, flags);
    } else {
        p_backward = dyn_fftw_plan_dft_1d(Narg, in, out, FFTW_BACKWARD, flags);
    }
    asForwardDouble = forward;
    zN = Narg;
}
//=============================================================================
static void
scomplex_fft_init(int Narg, bool forward)
{
    if (cN == Narg && asForwardSingle == forward) {
        return;
    }
    if (cN != 0) {
        if (pf_forward != nullptr) {
            dyn_fftwf_destroy_plan(pf_forward);
            pf_forward = nullptr;
        }
        if (pf_backward != nullptr) {
            dyn_fftwf_destroy_plan(pf_backward);
            pf_backward = nullptr;
        }
        dyn_fftwf_free(inf);
        dyn_fftwf_free(outf);
    }
    inf = (fftwf_complex*)dyn_fftwf_malloc(sizeof(fftwf_complex) * (size_t)Narg);
    outf = (fftwf_complex*)dyn_fftwf_malloc(sizeof(fftwf_complex) * (size_t)Narg);
    int flags = getFftFlags(currentFftMethod, Narg);
    if (forward) {
        pf_forward = dyn_fftwf_plan_dft_1d(Narg, inf, outf, FFTW_FORWARD, flags);
    } else {
        pf_backward = dyn_fftwf_plan_dft_1d(Narg, inf, outf, FFTW_BACKWARD, flags);
    }
    asForwardSingle = forward;
    cN = Narg;
}
//=============================================================================
static void
scomplex_fft_forward(int Narg, float* dp)
{
    scomplex_fft_init(Narg, true);
    memcpy(inf, dp, sizeOfSingle * (size_t)Narg * (size_t)2);
    dyn_fftwf_execute(pf_forward);
    memcpy(dp, outf, sizeOfSingle * (size_t)Narg * (size_t)2);
}
//=============================================================================
static void
scomplex_fft_backward(int Narg, single* dp)
{
    scomplex_fft_init(Narg, false);
    memcpy(inf, dp, sizeOfSingle * (size_t)Narg * (size_t)2);
    dyn_fftwf_execute(pf_backward);
    memcpy(dp, outf, sizeOfSingle * (size_t)Narg * (size_t)2);
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < ompIndexType(2) * ompIndexType(cN); i++) {
        dp[i] /= ((single)Narg);
    }
}
//=============================================================================
static void
dcomplex_fft_forward(int Narg, double* dp)
{
    dcomplex_fft_init(Narg, true);
    memcpy(in, dp, sizeOfDouble * (size_t)Narg * (size_t)2);
    dyn_fftw_execute(p_forward);
    memcpy(dp, out, sizeOfDouble * (size_t)Narg * (size_t)2);
}
//=============================================================================
static void
dcomplex_fft_backward(int Narg, double* dp)
{
    dcomplex_fft_init(Narg, false);
    memcpy(in, dp, sizeOfDouble * (size_t)Narg * (size_t)2);
    dyn_fftw_execute(p_backward);
    memcpy(dp, out, sizeOfDouble * (size_t)Narg * (size_t)2);
#if WITH_OPENMP
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < ompIndexType(2) * ompIndexType(zN); i++) {
        dp[i] /= ((double)Narg);
    }
}
//=============================================================================
ArrayOf
scomplexFFTW(const ArrayOf& X, indexType n, indexType dim, bool asInverse)
{
    bool needToConvertToComplex = (X.getDataClass() == NLS_SINGLE);
    Dimensions inDim = X.getDimensions();
    Dimensions outDim(inDim);
    outDim[dim] = n;
    indexType planecount = 1;
    indexType planesize = 1;
    indexType linesize = inDim[dim];
    for (indexType d = dim + 1; d < inDim.getLength(); d++) {
        planecount *= inDim[d];
    }
    for (indexType d = 0; d < dim; d++) {
        planesize *= inDim[d];
    }
    single* ob = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.getElementCount());
    outDim.simplify();
    ArrayOf res = ArrayOf(NLS_SCOMPLEX, outDim, ob);

    single* buffer = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, n);
    indexType copyIn = n;
    if (inDim[dim] < n) {
        copyIn = inDim[dim];
    }
    auto* datapointer = (single*)X.getDataPointer();
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)copyIn; k++) {
                indexType idxBuffer = 2 * k;
                indexType cplxOffset = needToConvertToComplex ? 1 : 2;
                indexType idxDataPointer
                    = cplxOffset * (i * planesize * linesize + j + k * planesize);
                buffer[idxBuffer] = datapointer[idxDataPointer];
                buffer[idxBuffer + 1]
                    = needToConvertToComplex ? 0 : datapointer[idxDataPointer + 1];
            }
            asInverse ? scomplex_fft_backward((int)n, buffer)
                      : scomplex_fft_forward((int)n, buffer);

#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)n; k++) {
                indexType idxOb = 2 * (i * planesize * n + j + k * planesize);
                ob[idxOb] = buffer[2 * k];
                ob[idxOb + 1] = buffer[2 * k + 1];
            }
        }
    }
    delete[] buffer;
    buffer = nullptr;
    if (res.allReal()) {
        res.promoteType(NLS_SINGLE);
    }
    return res;
}
//=============================================================================
ArrayOf
dcomplexFFTW(const ArrayOf& X, indexType n, indexType dim, bool asInverse)
{
    bool needToConvertToComplex = (X.getDataClass() == NLS_DOUBLE);
    Dimensions inDim = X.getDimensions();
    Dimensions outDim(inDim);
    outDim[dim] = n;
    indexType planecount = 1;
    indexType planesize = 1;
    indexType linesize = inDim[dim];
    for (indexType d = dim + 1; d < inDim.getLength(); d++) {
        planecount *= inDim[d];
    }
    for (indexType d = 0; d < dim; d++) {
        planesize *= inDim[d];
    }
    double* ob = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.getElementCount());
    outDim.simplify();
    ArrayOf res = ArrayOf(NLS_DCOMPLEX, outDim, ob);

    double* buffer = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, n);
    indexType copyIn = n;
    if (inDim[dim] < n) {
        copyIn = inDim[dim];
    }
    auto* datapointer = (double*)X.getDataPointer();
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)copyIn; k++) {
                indexType idxBuffer = 2 * k;
                indexType cplxOffset = needToConvertToComplex ? 1 : 2;
                indexType idxDataPointer
                    = cplxOffset * (i * planesize * linesize + j + k * planesize);
                buffer[idxBuffer] = datapointer[idxDataPointer];
                buffer[idxBuffer + 1]
                    = needToConvertToComplex ? 0 : datapointer[idxDataPointer + 1];
            }
            asInverse ? dcomplex_fft_backward((int)n, buffer)
                      : dcomplex_fft_forward((int)n, buffer);
#if WITH_OPENMP
#pragma omp parallel for
#endif
            for (ompIndexType k = 0; k < (ompIndexType)n; k++) {
                indexType idxOb = 2 * (i * planesize * n + j + k * planesize);
                indexType idxBuffer = 2 * k;
                ob[idxOb] = buffer[idxBuffer];
                ob[idxOb + 1] = buffer[idxBuffer + 1];
            }
        }
    }
    delete[] buffer;
    buffer = nullptr;
    if (res.allReal()) {
        res.promoteType(NLS_DOUBLE);
    }
    return res;
}
//=============================================================================
indexType
computeDim(const ArrayOf& X)
{
    indexType d = 0;
    if (X.isScalar()) {
        d = 1;
    } else {
        Dimensions dimsX = Dimensions(X.getDimensions());
        indexType len = dimsX.getLength();
        while (d < len && dimsX[d] == 1) {
            d++;
        }
        dimsX.simplify();
    }
    return d;
}
//=============================================================================
std::wstring
getDoubleWisdomInformation()
{
    char* buffer = dyn_fftw_export_wisdom_to_string();
    std::wstring res;
    if (buffer != nullptr) {
        res = utf8_to_wstring(buffer);
        /* According to the FFTW documentation, we should free 'buffer'
           string but doing makes Nelson crash :( !!!
        */
        // free(buffer);
    }
    return res;
}
//=============================================================================
std::wstring
getSingleWisdomInformation()
{
    char* buffer = dyn_fftwf_export_wisdom_to_string();
    std::wstring res;
    if (buffer != nullptr) {
        res = utf8_to_wstring(buffer);
        /* According to the FFTW documentation, we should free 'buffer'
        string but doing makes Nelson crash :( !!!
        */
        // free(buffer);
    }
    return res;
}
//=============================================================================
std::wstring
getPlannerInformation()
{
    std::wstring res;
    switch (currentFftMethod) {
    case ESTIMATE:
        return L"estimate";
    case MEASURE:
        return L"measure";
    case PATIENT:
        return L"patient";
    case EXHAUSTIVE:
        return L"exhaustive";
    case HYBRID:
        return L"hybrid";
    default: {
        return L"unknown";
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
