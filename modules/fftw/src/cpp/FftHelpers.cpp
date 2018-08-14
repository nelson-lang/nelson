//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include <cstring>
#include <fftw3.h>
#include "FftHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define DEFAULT_FFT_METHOD FftPlannerMethod::ESTIMATE
static FftPlannerMethod currentFftMethod = DEFAULT_FFT_METHOD;
//=============================================================================
static fftw_complex* in = nullptr;
static fftw_complex* out = nullptr;
static fftw_plan p_forward;
static fftw_plan p_backward;
static int zN = 0;
//=============================================================================
static fftwf_complex* inf = nullptr;
static fftwf_complex* outf = nullptr;
static fftwf_plan pf_forward;
static fftwf_plan pf_backward;
static int cN = 0;
//=============================================================================
static size_t sizeOfDouble = sizeof(double);
static size_t sizeOfSingle = sizeof(single);
//=============================================================================
static int
getFftFlags(FftPlannerMethod, int Narg)
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
setDoubleWisdomInformation(std::wstring info)
{
    std::string uinfo = wstring_to_utf8(info);
    return (fftw_import_wisdom_from_string(uinfo.c_str()) == 1);
}
//=============================================================================
bool
setSingleWisdomInformation(std::wstring info)
{
    std::string uinfo = wstring_to_utf8(info);
    return (fftwf_import_wisdom_from_string(uinfo.c_str()) == 1);
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
    fftw_forget_wisdom();
}
//=============================================================================
void
resetSingleWisdom()
{
    fftwf_forget_wisdom();
}
//=============================================================================
void
resetPlanner()
{
    setPlannerInformation(DEFAULT_FFT_METHOD);
}
//=============================================================================
static void
scomplex_fft_init(int Narg)
{
    if (cN == Narg) {
        return;
    }
    if (cN != 0) {
        fftwf_destroy_plan(pf_forward);
        fftwf_destroy_plan(pf_backward);
        fftwf_free(inf);
        fftwf_free(outf);
    }
    inf = (fftwf_complex*)fftwf_malloc(sizeof(fftwf_complex) * Narg);
    outf = (fftwf_complex*)fftwf_malloc(sizeof(fftwf_complex) * Narg);
    int flags = getFftFlags(currentFftMethod, Narg);
    pf_forward = fftwf_plan_dft_1d(Narg, inf, outf, FFTW_FORWARD, flags);
    pf_backward = fftwf_plan_dft_1d(Narg, inf, outf, FFTW_BACKWARD, flags);
    cN = Narg;
}
//=============================================================================
static void
scomplex_fft_forward(int Narg, float* dp)
{
    if (cN != Narg) {
        scomplex_fft_init(Narg);
    }
    memcpy(inf, dp, sizeOfSingle * Narg * 2);
    fftwf_execute(pf_forward);
    memcpy(dp, outf, sizeOfSingle * Narg * 2);
}
//=============================================================================
static void
scomplex_fft_backward(int Narg, single* dp)
{
    if (cN != Narg) {
        scomplex_fft_init(Narg);
    }
    memcpy(inf, dp, sizeOfSingle * Narg * 2);
    fftwf_execute(pf_backward);
    memcpy(dp, outf, sizeOfSingle * Narg * 2);
    for (int i = 0; i < (2 * cN); i++) {
        dp[i] /= ((single)Narg);
    }
}
//=============================================================================
static void
dcomplex_fft_init(int Narg)
{
    if (zN == Narg) {
        return;
    }
    if (zN != 0) {
        fftw_destroy_plan(p_forward);
        fftw_destroy_plan(p_backward);
        fftw_free(in);
        fftw_free(out);
    }
    in = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * Narg);
    out = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * Narg);
    int flags = getFftFlags(currentFftMethod, Narg);
    p_forward = fftw_plan_dft_1d(Narg, in, out, FFTW_FORWARD, flags);
    p_backward = fftw_plan_dft_1d(Narg, in, out, FFTW_BACKWARD, flags);
    zN = Narg;
}
//=============================================================================
static void
dcomplex_fft_forward(int Narg, double* dp)
{
    if (zN != Narg) {
        dcomplex_fft_init(Narg);
    }
    memcpy(in, dp, sizeOfDouble * Narg * 2);
    fftw_execute(p_forward);
    memcpy(dp, out, sizeOfDouble * Narg * 2);
}
//=============================================================================
static void
dcomplex_fft_backward(int Narg, double* dp)
{
    if (zN != Narg) {
        dcomplex_fft_init(Narg);
    }
    memcpy(in, dp, sizeOfDouble * Narg * 2);
    fftw_execute(p_backward);
    memcpy(dp, out, sizeOfDouble * Narg * 2);
    for (int i = 0; i < (2 * zN); i++) {
        dp[i] /= ((double)Narg);
    }
}
//=============================================================================
ArrayOf
scomplexFFTW(ArrayOf X, indexType n, indexType dim, bool asInverse)
{
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
    single* ob = (single*)ArrayOf::allocateArrayOf(
        NLS_SINGLE, 2 * outDim.getElementCount(), stringVector(), false);
    single* buffer = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, n * 2, stringVector(), true);
    indexType copyIn = n;
    if (inDim[dim] < n) {
        copyIn = inDim[dim];
    }
    single* datapointer = (single*)X.getDataPointer();
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            for (indexType k = 0; k < copyIn; k++) {
                indexType idxBuffer = 2 * k;
                indexType idxDataPointer = 2 * (i * planesize * linesize + j + k * planesize);
                buffer[idxBuffer] = datapointer[idxDataPointer];
                buffer[idxBuffer + 1] = datapointer[idxDataPointer + 1];
            }
            if (asInverse) {
                scomplex_fft_backward((int)n, buffer);
            } else {
                scomplex_fft_forward((int)n, buffer);
            }
            for (indexType k = 0; k < n; k++) {
                indexType idxOb = 2 * (i * planesize * n + j + k * planesize);
                ob[idxOb] = buffer[2 * k];
                ob[idxOb + 1] = buffer[2 * k + 1];
            }
        }
    }
    delete[] buffer;
    buffer = nullptr;
    outDim.simplify();
    ArrayOf res = ArrayOf(NLS_SCOMPLEX, outDim, ob);
    if (res.allReal()) {
        res.promoteType(NLS_SINGLE);
    }
    return res;
}
//=============================================================================
ArrayOf
dcomplexFFTW(ArrayOf X, indexType n, indexType dim, bool asInverse)
{
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
    double* ob = (double*)ArrayOf::allocateArrayOf(
        NLS_DOUBLE, 2 * outDim.getElementCount(), stringVector(), false);
    double* buffer = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, n * 2, stringVector(), true);
    indexType copyIn = n;
    if (inDim[dim] < n) {
        copyIn = inDim[dim];
    }
    double* datapointer = (double*)X.getDataPointer();
    for (indexType i = 0; i < planecount; i++) {
        for (indexType j = 0; j < planesize; j++) {
            for (indexType k = 0; k < copyIn; k++) {
                indexType idxBuffer = 2 * k;
                indexType idxDataPointer = 2 * (i * planesize * linesize + j + k * planesize);
                buffer[idxBuffer] = datapointer[idxDataPointer];
                buffer[idxBuffer + 1] = datapointer[idxDataPointer + 1];
            }
            if (asInverse) {
                dcomplex_fft_backward((int)n, buffer);
            } else {
                dcomplex_fft_forward((int)n, buffer);
            }
            for (indexType k = 0; k < n; k++) {
                indexType idxOb = 2 * (i * planesize * n + j + k * planesize);
                indexType idxBuffer = 2 * k;
                ob[idxOb] = buffer[idxBuffer];
                ob[idxOb + 1] = buffer[idxBuffer + 1];
            }
        }
    }
    delete[] buffer;
    buffer = nullptr;
    outDim.simplify();
    ArrayOf res = ArrayOf(NLS_DCOMPLEX, outDim, ob);
    if (res.allReal()) {
        res.promoteType(NLS_DOUBLE);
    }
    return res;
}
//=============================================================================
indexType
computeDim(const ArrayOf X)
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
    char* buffer = fftw_export_wisdom_to_string();
    std::wstring res;
    if (buffer) {
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
    char* buffer = fftwf_export_wisdom_to_string();
    std::wstring res;
    if (buffer) {
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
}
//=============================================================================
