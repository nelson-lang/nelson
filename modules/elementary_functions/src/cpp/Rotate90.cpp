//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "Rotate90.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static Dimensions
getOutputDimensions(int nbRotations, const Dimensions& dimsIn)
{
    Dimensions dimsOut(dimsIn);
    switch (nbRotations) {
    case 0:
    case 2: {
        dimsOut = Dimensions(dimsIn.getRows(), dimsIn.getColumns());
    } break;
    case 1:
    case 3: {
        dimsOut = Dimensions(dimsIn.getColumns(), dimsIn.getRows());
    } break;
    default: {
    } break;
    }
    return dimsOut;
}
//=============================================================================
template <class T>
ArrayOf
rotate2d90Real(NelsonType nlsType, T* ptrIn, const Dimensions& dimsIn, int nbIterations)
{
    Dimensions dimsOut = getOutputDimensions(nbIterations, dimsIn);
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(nlsType, dimsIn.getElementCount());
    ArrayOf res = ArrayOf(nlsType, dimsOut, ptrOut);

    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matIn(
        ptrIn, dimsIn.getRows(), dimsIn.getColumns());
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matOut(
        ptrOut, dimsOut.getRows(), dimsOut.getColumns());

    switch (nbIterations) {
    case 1: {
        matOut = matIn.transpose().colwise().reverse().eval();
    } break;
    case 2: {
        matOut = matIn.reverse().eval();
    } break;
    case 3: {
        matOut = matIn.transpose().rowwise().reverse().eval();
    } break;
    default: {
    } break;
    }
    return res;
}
//=============================================================================
template <class T>
ArrayOf
rotate2d90Complex(NelsonType nlsType, T* ptrIn, const Dimensions& dimsIn, int nbIterations)
{
    Dimensions dimsOut = getOutputDimensions(nbIterations, dimsIn);
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(nlsType, dimsIn.getElementCount());
    ArrayOf res = ArrayOf(nlsType, dimsOut, ptrOut);

    auto* pzIn = reinterpret_cast<std::complex<T>*>(ptrIn);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matIn(
        pzIn, dimsIn.getRows(), dimsIn.getColumns());

    auto* pzOut = reinterpret_cast<std::complex<T>*>(ptrOut);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matOut(
        pzOut, dimsOut.getRows(), dimsOut.getColumns());

    switch (nbIterations) {
    case 1: {
        matOut = matIn.transpose().colwise().reverse().eval();
    } break;
    case 2: {
        matOut = matIn.reverse().eval();
    } break;
    case 3: {
        matOut = matIn.transpose().rowwise().reverse().eval();
    } break;
    default: {
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
Rotate90(const ArrayOf& arrayIn, int nbRotations)
{
    if (arrayIn.isSparse() || !arrayIn.is2D()) {
        Error(_("Type not managed."));
    }

    if (nbRotations < 0 || nbRotations > 3) {
        Error(_("Input argument #1: scalar integer value expected."), "Nelson:rot90:kNonInteger");
    }
    if (nbRotations == 0) {
        ArrayOf res(arrayIn);
        res.ensureSingleOwner();
        return res;
    }

    if (arrayIn.isEmpty()) {
        Dimensions outputDimensions = getOutputDimensions(nbRotations, arrayIn.getDimensions());
        ArrayOf res = ArrayOf::emptyConstructor(outputDimensions);
        res.promoteType(arrayIn.getDataClass());
        return res;
    }

    switch (arrayIn.getDataClass()) {
    case NLS_LOGICAL: {
        return rotate2d90Real<logical>(
            NLS_LOGICAL, (logical*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_UINT8: {
        return rotate2d90Real<uint8>(
            NLS_UINT8, (uint8*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_INT8: {
        return rotate2d90Real<int8>(
            NLS_INT8, (int8*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_UINT16: {
        return rotate2d90Real<uint16>(
            NLS_UINT16, (uint16*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_INT16: {
        return rotate2d90Real<int16>(
            NLS_INT16, (int16*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_UINT32: {
        return rotate2d90Real<uint32>(
            NLS_UINT32, (uint32*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_INT32: {
        return rotate2d90Real<int32>(
            NLS_INT32, (int32*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_UINT64: {
        return rotate2d90Real<uint64>(
            NLS_UINT64, (uint64*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_INT64: {
        return rotate2d90Real<int64>(
            NLS_INT64, (int64*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_SINGLE: {
        return rotate2d90Real<single>(
            NLS_SINGLE, (single*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_DOUBLE: {
        return rotate2d90Real<double>(
            NLS_DOUBLE, (double*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_CHAR: {
        return rotate2d90Real<wchar_t>(
            NLS_CHAR, (wchar_t*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_SCOMPLEX: {
        return rotate2d90Complex<single>(
            NLS_SCOMPLEX, (single*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    case NLS_DCOMPLEX: {
        return rotate2d90Complex<double>(
            NLS_DCOMPLEX, (double*)arrayIn.getDataPointer(), arrayIn.getDimensions(), nbRotations);
    } break;
    default: {
        Error(_("Type not managed."));
    } break;
    }
    return {};
}
//=============================================================================
}
//=============================================================================
