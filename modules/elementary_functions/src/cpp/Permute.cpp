//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <cstdint>
#include "Permute.hpp"
#include "Transpose.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
permuteReferenceType(ArrayOf& src, void* dst_data, indexType nd, indexType nOuterBlocks,
    indexType runLen, const indexType* outLen, const indexType* sStride);
static void
permutePodType(const char* sp, char* qp, size_t es, indexType nd, indexType nOuterBlocks,
    indexType runLen, const indexType* outLen, const indexType* sStride);
//=============================================================================
ArrayOf
Permute(
    const ArrayOf& arrayIn, const std::vector<indexType>& permutationVector, bool& needToOverload)
{
    ArrayOf res;
    bool isSupportedTypes = (arrayIn.dataClass() <= NLS_CHAR) && !arrayIn.isSparse()
        || arrayIn.isStringArray() || arrayIn.isCell() || (arrayIn.isStruct())
        || arrayIn.isClassType() || arrayIn.isHandle();
    if (!isSupportedTypes) {
        needToOverload = true;
        return arrayIn;
    }
    Dimensions dimsIn = arrayIn.dimensions();

    indexType Adims = dimsIn.length();
    bool id_perm = true;
    for (indexType i = 0; i < Adims; i++) {
        if ((permutationVector[i] - 1) != (indexType)i) {
            id_perm = false;
        }
    }
    if (id_perm) {
        return arrayIn;
    }

    if (arrayIn.isEmpty()) {
        if (dimsIn.length() < permutationVector.size()) {
            for (indexType k = dimsIn.length(); k < (indexType)permutationVector.size(); k++) {
                dimsIn.setDimensionLength(k, 1);
            }
        }
        Dimensions newdims(dimsIn.permute(permutationVector, true));
        newdims.simplify();
        // an empty array keeps its class (fields, object class) once permuted
        ArrayOf permuted(arrayIn);
        permuted.reshape(newdims);
        return permuted;
    }

    if (arrayIn.isScalar()) {
        return arrayIn;
    }

    if (arrayIn.is2D()) {
        bool isTranspose = true;
        for (int i = 2; i < dimsIn.length(); i++) {
            if (permutationVector[i] != (i + 1)) {
                isTranspose = false;
            }
        }
        if (permutationVector[0] != 2) {
            isTranspose = false;
        }
        if (permutationVector[1] != 1) {
            isTranspose = false;
        }
        if (isTranspose) {
            return Transpose(arrayIn, needToOverload);
        }
        // A bare reshape keeps the element layout only when dimension 1 still
        // precedes dimension 2 in the output, or one of them is a singleton.
        // Otherwise the data must be moved by the general N-D path below.
        indexType pos1 = 0;
        indexType pos2 = 0;
        for (indexType k = 0; k < (indexType)permutationVector.size(); ++k) {
            if (permutationVector[k] == 1) {
                pos1 = k;
            } else if (permutationVector[k] == 2) {
                pos2 = k;
            }
        }
        bool reshapeSafe
            = (dimsIn.dimensionLength(0) == 1) || (dimsIn.dimensionLength(1) == 1) || (pos1 < pos2);
        if (reshapeSafe) {
            res = arrayIn;
            Dimensions newdims(dimsIn.permute(permutationVector, false));
            res.reshape(newdims);
            return res;
        }
    }

    Dimensions newdims(dimsIn.permute(permutationVector, false));

    void* dst_data = ArrayOf::allocateArrayOf(
        arrayIn.dataClass(), newdims.elementCount(), arrayIn.fieldNames(), true);

    newdims.simplify();
    res = ArrayOf(arrayIn.dataClass(), newdims, dst_data, arrayIn.isSparse(), arrayIn.fieldNames());
    if (arrayIn.isClassType()) {
        res.setClassType(arrayIn.classType());
        if (arrayIn.dataClass() == NLS_CLASSDEF_ARRAY) {
            res.setClassdefLayout(arrayIn.classdefLayout());
        }
    }

    // The output is written in linear order. A block is a full sweep of output
    // dimension 0 (runLen elements); blocks are independent, run on separate
    // threads, and each computes its own source base offset from the higher
    // dimensions.
    const indexType nd = (indexType)permutationVector.size();
    indexType inLen[MAXDIMS];
    indexType inStride[MAXDIMS];
    indexType outLen[MAXDIMS];
    indexType sStride[MAXDIMS];
    indexType acc = 1;
    for (indexType d = 0; d < nd; ++d) {
        inLen[d] = dimsIn.dimensionLength(d);
        inStride[d] = acc;
        acc *= inLen[d];
    }
    for (indexType k = 0; k < nd; ++k) {
        indexType srcDim = permutationVector[k] - 1;
        outLen[k] = inLen[srcDim];
        sStride[k] = inStride[srcDim];
    }

    indexType runLen = outLen[0];
    indexType nOuterBlocks = 1;
    for (indexType k = 1; k < nd; ++k) {
        nOuterBlocks *= outLen[k];
    }

    if (arrayIn.isCell() || arrayIn.isStringArray() || arrayIn.isStruct() || arrayIn.isClassType()
        || arrayIn.isHandle()) {
        ArrayOf src = arrayIn;
        permuteReferenceType(src, dst_data, nd, nOuterBlocks, runLen, outLen, sStride);
    } else {
        const char* sp = (const char*)arrayIn.dataPointer();
        char* qp = (char*)dst_data;
        size_t es = (size_t)arrayIn.elementSize();
        permutePodType(sp, qp, es, nd, nOuterBlocks, runLen, outLen, sStride);
    }
    return res;
}
//=============================================================================
// 16-byte element (double/single complex) block for the typed tiled kernel.
struct Blk16
{
    uint64_t a;
    uint64_t b;
};
//=============================================================================
// Cache-blocked transpose kernel. The two "special" axes are output axis 0
// (contiguous writes, source stride sStrideW) and the output axis that carries
// source axis 0 (contiguous reads, output stride dstStrideR). Both are walked
// in TILE-sized blocks so the strided side stays cache-resident. Every other
// output axis is a "page"; pages and tiles are flattened into one OpenMP loop
// so the parallel iteration count stays above the OpenMP threshold even when
// individual axes are short.
template <typename T>
static void
tiledKernel(const T* sp, T* qp, indexType W, indexType sStrideW, indexType R, indexType sStrideR,
    indexType dstStrideR, const indexType* pageOutLen, const indexType* pageSStride,
    const indexType* pageDstStride, indexType nPageDims, indexType nPages)
{
    const indexType TILE = 64;
    indexType nTileW = (W + TILE - 1) / TILE;
    indexType nTileR = (R + TILE - 1) / TILE;
    indexType tilesPerPage = nTileW * nTileR;
    indexType total = nPages * tilesPerPage;
    OMP_PARALLEL_FOR_LOOP(total)
    for (ompIndexType o = 0; o < (ompIndexType)total; ++o) {
        indexType pageIdx = (indexType)o / tilesPerPage;
        indexType t = (indexType)o % tilesPerPage;
        indexType iw0 = (t / nTileR) * TILE;
        indexType iwe = (iw0 + TILE < W) ? iw0 + TILE : W;
        indexType ir0 = (t % nTileR) * TILE;
        indexType ire = (ir0 + TILE < R) ? ir0 + TILE : R;
        indexType rem = pageIdx;
        indexType pSrc = 0;
        indexType pDst = 0;
        for (indexType d = 0; d < nPageDims; ++d) {
            indexType c = rem % pageOutLen[d];
            rem /= pageOutLen[d];
            pSrc += c * pageSStride[d];
            pDst += c * pageDstStride[d];
        }
        for (indexType ir = ir0; ir < ire; ++ir) {
            const T* srcCol = sp + pSrc + ir * sStrideR;
            T* dstCol = qp + pDst + ir * dstStrideR;
            for (indexType iw = iw0; iw < iwe; ++iw) {
                dstCol[iw] = srcCol[iw * sStrideW];
            }
        }
    }
}
//=============================================================================
// Byte-granularity fallback of tiledKernel for uncommon element sizes.
static void
tiledKernelBytes(const char* sp, char* qp, size_t es, indexType W, indexType sStrideW, indexType R,
    indexType sStrideR, indexType dstStrideR, const indexType* pageOutLen,
    const indexType* pageSStride, const indexType* pageDstStride, indexType nPageDims,
    indexType nPages)
{
    const indexType TILE = 64;
    indexType nTileW = (W + TILE - 1) / TILE;
    indexType nTileR = (R + TILE - 1) / TILE;
    indexType tilesPerPage = nTileW * nTileR;
    indexType total = nPages * tilesPerPage;
    OMP_PARALLEL_FOR_LOOP(total)
    for (ompIndexType o = 0; o < (ompIndexType)total; ++o) {
        indexType pageIdx = (indexType)o / tilesPerPage;
        indexType t = (indexType)o % tilesPerPage;
        indexType iw0 = (t / nTileR) * TILE;
        indexType iwe = (iw0 + TILE < W) ? iw0 + TILE : W;
        indexType ir0 = (t % nTileR) * TILE;
        indexType ire = (ir0 + TILE < R) ? ir0 + TILE : R;
        indexType rem = pageIdx;
        indexType pSrc = 0;
        indexType pDst = 0;
        for (indexType d = 0; d < nPageDims; ++d) {
            indexType c = rem % pageOutLen[d];
            rem /= pageOutLen[d];
            pSrc += c * pageSStride[d];
            pDst += c * pageDstStride[d];
        }
        for (indexType ir = ir0; ir < ire; ++ir) {
            indexType srcCol = pSrc + ir * sStrideR;
            indexType dstCol = pDst + ir * dstStrideR;
            for (indexType iw = iw0; iw < iwe; ++iw) {
                std::memcpy(qp + (size_t)(dstCol + iw) * es,
                    sp + (size_t)(srcCol + iw * sStrideW) * es, es);
            }
        }
    }
}
//=============================================================================
// Set up the tiled transpose: identify the read/write axes and the page axes,
// then dispatch to a typed kernel for the common element sizes.
static void
permutePodTiled(const char* sp, char* qp, size_t es, indexType nd, const indexType* outLen,
    const indexType* sStride)
{
    indexType outStride[MAXDIMS];
    indexType a = 1;
    for (indexType k = 0; k < nd; ++k) {
        outStride[k] = a;
        a *= outLen[k];
    }
    // output axis that reads source axis 0 (its source stride is 1)
    indexType q = 0;
    for (indexType k = 0; k < nd; ++k) {
        if (sStride[k] == 1) {
            q = k;
            break;
        }
    }
    indexType W = outLen[0];
    indexType sStrideW = sStride[0];
    indexType R = outLen[q];
    indexType sStrideR = sStride[q];
    indexType dstStrideR = outStride[q];

    indexType pageOutLen[MAXDIMS];
    indexType pageSStride[MAXDIMS];
    indexType pageDstStride[MAXDIMS];
    indexType nPageDims = 0;
    indexType nPages = 1;
    for (indexType k = 1; k < nd; ++k) {
        if (k == q) {
            continue;
        }
        pageOutLen[nPageDims] = outLen[k];
        pageSStride[nPageDims] = sStride[k];
        pageDstStride[nPageDims] = outStride[k];
        nPages *= outLen[k];
        ++nPageDims;
    }

    switch (es) {
    case 1:
        tiledKernel<uint8_t>((const uint8_t*)sp, (uint8_t*)qp, W, sStrideW, R, sStrideR, dstStrideR,
            pageOutLen, pageSStride, pageDstStride, nPageDims, nPages);
        break;
    case 2:
        tiledKernel<uint16_t>((const uint16_t*)sp, (uint16_t*)qp, W, sStrideW, R, sStrideR,
            dstStrideR, pageOutLen, pageSStride, pageDstStride, nPageDims, nPages);
        break;
    case 4:
        tiledKernel<uint32_t>((const uint32_t*)sp, (uint32_t*)qp, W, sStrideW, R, sStrideR,
            dstStrideR, pageOutLen, pageSStride, pageDstStride, nPageDims, nPages);
        break;
    case 8:
        tiledKernel<uint64_t>((const uint64_t*)sp, (uint64_t*)qp, W, sStrideW, R, sStrideR,
            dstStrideR, pageOutLen, pageSStride, pageDstStride, nPageDims, nPages);
        break;
    case 16:
        tiledKernel<Blk16>((const Blk16*)sp, (Blk16*)qp, W, sStrideW, R, sStrideR, dstStrideR,
            pageOutLen, pageSStride, pageDstStride, nPageDims, nPages);
        break;
    default:
        tiledKernelBytes(sp, qp, es, W, sStrideW, R, sStrideR, dstStrideR, pageOutLen, pageSStride,
            pageDstStride, nPageDims, nPages);
        break;
    }
}
//=============================================================================
// POD (numeric / char / logical / complex-as-interleaved) fast path. When
// output dimension 0 maps to a unit source stride each block is one contiguous
// memcpy; otherwise the copy is a strided transpose handled by cache tiling.
static void
permutePodType(const char* sp, char* qp, size_t es, indexType nd, indexType nOuterBlocks,
    indexType runLen, const indexType* outLen, const indexType* sStride)
{
    if (sStride[0] == 1) {
        OMP_PARALLEL_FOR_LOOP(nOuterBlocks)
        for (ompIndexType b = 0; b < (ompIndexType)nOuterBlocks; ++b) {
            indexType rem = (indexType)b;
            indexType srcBase = 0;
            for (indexType k = 1; k < nd; ++k) {
                indexType c = rem % outLen[k];
                rem /= outLen[k];
                srcBase += c * sStride[k];
            }
            std::memcpy(
                qp + (indexType)b * runLen * es, sp + (size_t)srcBase * es, (size_t)runLen * es);
        }
    } else {
        permutePodTiled(sp, qp, es, nd, outLen, sStride);
    }
}
//=============================================================================
// Reference types (cell / string / struct / classdef / handle) go through
// copyElements, which manages element semantics; kept single-threaded but
// still uses the same block decomposition to avoid per-element index rebuilds.
static void
permuteReferenceType(ArrayOf& src, void* dst_data, indexType nd, indexType nOuterBlocks,
    indexType runLen, const indexType* outLen, const indexType* sStride)
{
    indexType dim0Stride = sStride[0];
    for (indexType b = 0; b < nOuterBlocks; ++b) {
        indexType rem = b;
        indexType srcBase = 0;
        for (indexType k = 1; k < nd; ++k) {
            indexType c = rem % outLen[k];
            rem /= outLen[k];
            srcBase += c * sStride[k];
        }
        indexType dstBase = b * runLen;
        indexType srcOff = srcBase;
        for (indexType m = 0; m < runLen; ++m) {
            src.copyElements(srcOff, dst_data, dstBase + m, 1);
            srcOff += dim0Stride;
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
