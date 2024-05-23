//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#define EIGEN_NO_DEBUG
#include <Eigen/Dense>
#include "repmatBuiltin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
ArrayOf
scalarRealRepmat(const ArrayOf& x, void* dp, const Dimensions& outDims)
{
    const T* ptrValue = static_cast<const T*>(x.getDataPointer());
    T value = ptrValue[0];
    T* ptrA = static_cast<T*>(dp);
    ompIndexType nbElements = (ompIndexType)outDims.getElementCount();
    if (nbElements == 1) {
        ptrA[0] = value;
    } else {
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
            ptrA[k] = value;
        }
    }
    return ArrayOf(x.getDataClass(), outDims, dp, false);
}
//=============================================================================
template <class T>
ArrayOf
scalarComplexRepmat(const ArrayOf& x, void* dp, const Dimensions& outDims)
{
    auto* ptrXz = reinterpret_cast<std::complex<T>*>((T*)x.getDataPointer());
    auto* ptrDpz = reinterpret_cast<std::complex<T>*>((T*)dp);

    std::complex<T> value = ptrXz[0];
    ompIndexType nbElements = (ompIndexType)outDims.getElementCount();
    if (nbElements == 1) {
        ptrDpz[0] = value;
    } else {
#if WITH_OPENMP
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
            ptrDpz[k] = value;
        }
    }
    return ArrayOf(x.getDataClass(), outDims, dp, false);
}
//=============================================================================
template <class T>
ArrayOf
matrix2DRealRepmat(const ArrayOf& x, void* dp, const Dimensions& outDims)
{
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matIn(
        (T*)x.getDataPointer(), x.getRows(), x.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOut(
        (T*)dp, outDims.getRows(), outDims.getColumns());
    matOut = matIn.replicate(outDims.getRows(), outDims.getColumns());
    return ArrayOf(x.getDataClass(), outDims, dp, false);
}
//=============================================================================
template <class T>
ArrayOf
matrix2DComplexRepmat(const ArrayOf& x, void* dp, const Dimensions& outDims)
{
    auto* ptrXz = reinterpret_cast<std::complex<T>*>((T*)x.getDataPointer());
    auto* ptrDpz = reinterpret_cast<std::complex<T>*>((T*)dp);

    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matIn(
        ptrXz, x.getRows(), x.getColumns());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matOut(
        ptrDpz, outDims.getRows(), outDims.getColumns());
    matOut = matIn.replicate(outDims.getRows(), outDims.getColumns());
    return ArrayOf(x.getDataClass(), outDims, dp, false);
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::repmatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // R = repmat(A, m)
    // R = repmat(A, m, n)
    // R = repmat(A, m, n, p, ...)
    // R = repmat(A, [m n])
    // R = repmat(A, [m n p ...])
    ArrayOfVector retval;
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    Dimensions repcount;
    ArrayOf x = argIn[0];
    NelsonType classx = x.getDataClass();
    bool isNotSupportedType = (classx == NLS_HANDLE || classx == NLS_GO_HANDLE || x.isSparse());
    if (isNotSupportedType) {
        Error(ERROR_TYPE_NOT_SUPPORTED);
    }
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        if (param2.isScalar()) {
            repcount[0] = (indexType)param2.getContentAsUnsignedInteger64Scalar();
            repcount[1] = (indexType)param2.getContentAsUnsignedInteger64Scalar();
        } else {
            if (param2.isRowVector()) {
                param2.promoteType(NLS_UINT64);
                if (param2.getElementCount() > maxDims) {
                    Error(_W("Too many dimensions!"));
                }
                auto* dp = (uint64*)param2.getDataPointer();
                for (indexType i = 0; i < param2.getElementCount(); i++) {
                    repcount[i] = (indexType)dp[i];
                }
            } else {
                Error(_W("An row vector expected."));
            }
        }
    } else {
        for (size_t k = 1; k < argIn.size(); ++k) {
            ArrayOf paramK = argIn[k];
            repcount[k - 1] = (indexType)paramK.getContentAsUnsignedInteger64Scalar();
        }
    }
    Dimensions originalSize(x.getDimensions());
    indexType outdim = (repcount.getLength()) > (originalSize.getLength())
        ? (repcount.getLength())
        : (originalSize.getLength());
    Dimensions outdims;
    for (indexType i = 0; i < outdim; i++) {
        outdims[i] = originalSize[i] * repcount[i];
    }
    outdims.simplify();
    void* dp
        = ArrayOf::allocateArrayOf(classx, outdims.getElementCount(), x.getFieldNames(), false);
    indexType colsize = originalSize[0];
    if (!outdims.isEmpty(true)) {
        if (x.isScalar() && classx <= NLS_CHAR) {
            switch (classx) {
            case NLS_DOUBLE: {
                return scalarRealRepmat<double>(x, dp, outdims);
            } break;
            case NLS_SINGLE: {
                return scalarRealRepmat<single>(x, dp, outdims);
            } break;
            case NLS_DCOMPLEX: {
                return scalarComplexRepmat<double>(x, dp, outdims);
            } break;
            case NLS_SCOMPLEX: {
                return scalarComplexRepmat<single>(x, dp, outdims);
            } break;
            case NLS_INT8: {
                return scalarRealRepmat<int8>(x, dp, outdims);
            } break;
            case NLS_INT16: {
                return scalarRealRepmat<int16>(x, dp, outdims);
            } break;
            case NLS_INT32: {
                return scalarRealRepmat<int32>(x, dp, outdims);
            } break;
            case NLS_INT64: {
                return scalarRealRepmat<int64>(x, dp, outdims);
            } break;
            case NLS_UINT8: {
                return scalarRealRepmat<uint8>(x, dp, outdims);
            } break;
            case NLS_UINT16: {
                return scalarRealRepmat<uint16>(x, dp, outdims);
            } break;
            case NLS_UINT32: {
                return scalarRealRepmat<uint32>(x, dp, outdims);
            } break;
            case NLS_UINT64: {
                return scalarRealRepmat<uint64>(x, dp, outdims);
            } break;
            case NLS_LOGICAL: {
                return scalarRealRepmat<logical>(x, dp, outdims);
            } break;
            case NLS_CHAR: {
                return scalarRealRepmat<charType>(x, dp, outdims);
            } break;
            }
        } else {
            if ((x.isVector() || x.is2D()) && outdims.is2D() && classx <= NLS_CHAR) {
                switch (classx) {
                case NLS_DOUBLE: {
                    return matrix2DRealRepmat<double>(x, dp, outdims);
                } break;
                case NLS_SINGLE: {
                    return matrix2DRealRepmat<single>(x, dp, outdims);
                } break;
                case NLS_DCOMPLEX: {
                    return matrix2DComplexRepmat<double>(x, dp, outdims);
                } break;
                case NLS_SCOMPLEX: {
                    return matrix2DComplexRepmat<single>(x, dp, outdims);
                } break;
                case NLS_INT8: {
                    return matrix2DRealRepmat<int8>(x, dp, outdims);
                } break;
                case NLS_INT16: {
                    return matrix2DRealRepmat<int16>(x, dp, outdims);
                } break;
                case NLS_INT32: {
                    return matrix2DRealRepmat<int32>(x, dp, outdims);
                } break;
                case NLS_INT64: {
                    return matrix2DRealRepmat<int64>(x, dp, outdims);
                } break;
                case NLS_UINT8: {
                    return matrix2DRealRepmat<uint8>(x, dp, outdims);
                } break;
                case NLS_UINT16: {
                    return matrix2DRealRepmat<uint16>(x, dp, outdims);
                } break;
                case NLS_UINT32: {
                    return matrix2DRealRepmat<uint32>(x, dp, outdims);
                } break;
                case NLS_UINT64: {
                    return matrix2DRealRepmat<uint64>(x, dp, outdims);
                } break;
                case NLS_LOGICAL: {
                    return matrix2DRealRepmat<logical>(x, dp, outdims);
                } break;
                case NLS_CHAR: {
                    return matrix2DRealRepmat<charType>(x, dp, outdims);
                } break;
                }
            } else {
                indexType colcount = originalSize.getElementCount() / colsize;
                Dimensions copySelection(outdim);
                Dimensions sourceDimensions(originalSize.getLength());
                indexType copyCount = repcount.getElementCount();
                Dimensions anchor(outdim);
                for (indexType i = 0; i < copyCount; i++) {
                    sourceDimensions.zeroOut();
                    for (indexType j = 0; j < colcount; j++) {
                        for (ompIndexType k = 0; k < (ompIndexType)outdim; k++) {
                            anchor[k] = copySelection[k] * originalSize[k] + sourceDimensions[k];
                        }
                        x.copyElements(j * colsize, dp, outdims.mapPoint(anchor), colsize);
                        sourceDimensions.incrementModulo(originalSize, 1);
                    }
                    copySelection.incrementModulo(repcount, 0);
                }
            }
        }
    }
    ArrayOf res;
    if (classx == NLS_STRUCT_ARRAY || classx == NLS_CLASS_ARRAY) {
        res = ArrayOf(classx, outdims, dp, false, x.getFieldNames());
    } else {
        res = ArrayOf(classx, outdims, dp, false);
    }
    retval << res;
    return retval;
}
//=============================================================================
