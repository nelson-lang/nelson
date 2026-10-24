//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "ClassName.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
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

    OMP_PARALLEL_FOR_LOOP(cN)
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
    OMP_PARALLEL_FOR_LOOP(zN)
    for (ompIndexType i = 0; i < ompIndexType(2) * ompIndexType(zN); i++) {
        dp[i] /= ((double)Narg);
    }
}
//=============================================================================
ArrayOf
scomplexFFTW(const ArrayOf& X, indexType n, indexType dim, bool asInverse)
{
    bool needToConvertToComplex = (X.dataClass() == NLS_SINGLE);
    Dimensions inDim = X.dimensions();
    Dimensions outDim(inDim);
    outDim[dim] = n;
    indexType planecount = 1;
    indexType planesize = 1;
    indexType linesize = inDim[dim];
    for (indexType d = dim + 1; d < inDim.length(); d++) {
        planecount *= inDim[d];
    }
    for (indexType d = 0; d < dim; d++) {
        planesize *= inDim[d];
    }
    single* ob = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.elementCount());
    outDim.simplify();
    ArrayOf res = ArrayOf(NLS_SCOMPLEX, outDim, ob);

    // zero-initialized so that lines shorter than n are zero-padded
    single* buffer = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, n, stringVector(), true);
    indexType copyIn = n;
    if (inDim[dim] < n) {
        copyIn = inDim[dim];
    }
    auto* datapointer = (single*)X.dataPointer();
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            // The transform is done in place and destroys the zero padding, so
            // the tail must be re-zeroed for every line, not just once.
            for (indexType k = copyIn; k < n; k++) {
                buffer[2 * k] = 0;
                buffer[2 * k + 1] = 0;
            }
            OMP_PARALLEL_FOR_LOOP(copyIn)
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

            OMP_PARALLEL_FOR_LOOP(n)
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
    bool needToConvertToComplex = (X.dataClass() == NLS_DOUBLE);
    Dimensions inDim = X.dimensions();
    Dimensions outDim(inDim);
    outDim[dim] = n;
    indexType planecount = 1;
    indexType planesize = 1;
    indexType linesize = inDim[dim];
    for (indexType d = dim + 1; d < inDim.length(); d++) {
        planecount *= inDim[d];
    }
    for (indexType d = 0; d < dim; d++) {
        planesize *= inDim[d];
    }
    double* ob = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.elementCount());
    outDim.simplify();
    ArrayOf res = ArrayOf(NLS_DCOMPLEX, outDim, ob);

    // zero-initialized so that lines shorter than n are zero-padded
    double* buffer = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, n, stringVector(), true);
    indexType copyIn = n;
    if (inDim[dim] < n) {
        copyIn = inDim[dim];
    }
    auto* datapointer = (double*)X.dataPointer();
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            // The transform is done in place and destroys the zero padding, so
            // the tail must be re-zeroed for every line, not just once.
            for (indexType k = copyIn; k < n; k++) {
                buffer[2 * k] = 0;
                buffer[2 * k + 1] = 0;
            }
            OMP_PARALLEL_FOR_LOOP(copyIn)
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
            OMP_PARALLEL_FOR_LOOP(n)
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
ArrayOf
prepareFftwInput(const ArrayOf& X, const std::string& functionName)
{
    if (X.isReferenceType() || X.isHandle() || X.isSparse()) {
        Error("Nelson:UndefinedFunction:undefinedFunctionValueInputArgumentsType",
            "Undefined function '%s' for input arguments of type '%s'.", functionName,
            ClassName(X));
    }
    switch (X.dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX:
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        return X;
    } break;
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32: {
        ArrayOf promoted(X);
        promoted.promoteType(NLS_DOUBLE);
        return promoted;
    } break;
    default: {
        Error("Nelson:fftw:invalidDataTypeFirst",
            "Invalid data type. First argument must be double, single, int8, uint8, int16, uint16, "
            "int32, uint32, or logical.");
    } break;
    }
    return {};
}
//=============================================================================
ArrayOf
emptyFftwResult(const ArrayOf& X, indexType n, indexType dim)
{
    NelsonType outputClass
        = (X.dataClass() == NLS_SINGLE || X.dataClass() == NLS_SCOMPLEX) ? NLS_SINGLE : NLS_DOUBLE;
    Dimensions outDim(X.dimensions());
    outDim[dim] = n;
    if (outDim.elementCount() == 0) {
        ArrayOf res = ArrayOf::empty(outDim);
        res.promoteType(outputClass);
        return res;
    }
    // empty input padded to a non-empty length: result is all zeros
    void* ptr = ArrayOf::allocateArrayOf(outputClass, outDim.elementCount(), stringVector(), true);
    return ArrayOf(outputClass, outDim, ptr);
}
//=============================================================================
indexType
computeDim(const ArrayOf& X)
{
    indexType d = 0;
    if (X.isScalar()) {
        d = 1;
    } else {
        Dimensions dimsX = Dimensions(X.dimensions());
        indexType len = dimsX.length();
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
        // FFTW documents that 'buffer' must be freed, but freeing it crashes Nelson.
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
        // FFTW documents that 'buffer' must be freed, but freeing it crashes Nelson.
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
