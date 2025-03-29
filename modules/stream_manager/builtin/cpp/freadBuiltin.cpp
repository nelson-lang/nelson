//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "freadBuiltin.hpp"
#include "Endian.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileRead.hpp"
#include "FilesManager.hpp"
#include "helpers.hpp"
#include "NelsonConfiguration.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// fread(fd, sz, 'double', sk,  'b')
// fread(fd, sz, 'double', 'b')
// fread(fd, sz, 'double', sk)
// fread(fd, 'double', 'b')
// fread(fd, sz, sk)
// fread(fd, sz, 'double')
// fread(fd, 'double')
// fread(fd, sz) --> fread(fd, sz, 'double')
//=============================================================================
static ArrayOfVector
freadBuiltinFiveRhs(int nLhs, const ArrayOfVector& argIn)
{
    bool bIsLittleEndian = true;
    size_t skipSize = 0;
    NelsonType classDest = NLS_UINT8;
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    ArrayOf param4 = argIn[3];
    ArrayOf param5 = argIn[4];
    std::wstring arg = param5.getContentAsWideString();
    if ((arg == L"n") || (arg == L"native")) {
        bIsLittleEndian = isLittleEndianFormat();
    } else if ((arg == L"b") || (arg == L"ieee-be")) {
        bIsLittleEndian = false;
    } else if ((arg == L"l") || (arg == L"ieee-le")) {
        bIsLittleEndian = true;
    } else {
        Error(_W("Wrong value for machine format."));
    }
    skipSize = static_cast<size_t>(param4.getContentAsScalarIndex());
    if (param3.isRowVectorCharacterArray()) {
        std::wstring precisionStr = param3.getContentAsWideString();
        bool bOK = false;
        classDest = precisionFromString(precisionStr, bOK);
        if (!bOK) {
            Error(_W("Wrong value for #3 argument: not supported precision."));
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_3_TYPE_STRING_EXPECTED);
    }
    if (param1.isDoubleType()) {
        if (!param2.isNumeric()) {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_NUMERIC_EXPECTED);
        }
        bool bSizeIs2D = param2.is2D() && !param2.isScalar();
        int64 isize = 0;
        int64 im = 0;
        int64 in = 0;
        if (bSizeIs2D) {
            // [m, n]
            // [m, Inf]
            param2.promoteType(NLS_DOUBLE);
            auto* dValues = static_cast<double*>(param2.getReadWriteDataPointer());
            double m = dValues[0];
            double n = dValues[1];
            if (std::isinf(m)) {
                Error(ERROR_WRONG_ARGUMENT_2_INVALID_VECTOR_SIZE);
            }
            im = static_cast<int64>(m);
            if (std::isinf(n)) {
                if (n > 0) {
                    isize = -1;
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_INVALID_VECTOR_SIZE);
                }
            } else {
                in = static_cast<int64>(n);
                isize = static_cast<int64>(m * n);
            }
        } else {
            auto dsize = param2.getContentAsDoubleScalar();
            if (std::isinf(dsize)) {
                if (dsize > 0) {
                    isize = -1;
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_INVALID_VECTOR_SIZE);
                }
            } else {
                isize = static_cast<int64>(dsize);
            }
        }
        auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
        auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
        if (fm == nullptr) {
            Error(_W("Problem with file manager."));
        }
        if (fm->isOpened(iValue)) {
            File* f = fm->getFile(iValue);
            int sizeReallyRead = -1;
            ArrayOf toRead
                = FileRead(f, isize, classDest, skipSize, bIsLittleEndian, sizeReallyRead);
            if (sizeReallyRead != -1) {
                if (bSizeIs2D) {
                    Dimensions dim;
                    if (isize == -1) {
                        // n is inf
                        in = sizeReallyRead / im;
                        if (sizeReallyRead % im) {
                            in++;
                        }
                        Dimensions dimL(static_cast<indexType>(im), static_cast<indexType>(in));
                        dim = dimL;
                    } else {
                        Dimensions dimL(static_cast<indexType>(im), static_cast<indexType>(in));
                        dim = dimL;
                    }
                    if (sizeReallyRead == im * in) {
                        toRead.reshape(dim);
                        retval << toRead;
                    } else {
                        void* ptr = ArrayOf::allocateArrayOf(toRead.getDataClass(),
                            static_cast<indexType>(im * in), stringVector(), false);
                        memcpy(ptr, toRead.getReadWriteDataPointer(), toRead.getByteSize());
                        retval << ArrayOf(toRead.getDataClass(), dim, ptr);
                    }
                } else {
                    retval << toRead;
                }
                if (nLhs > 1) {
                    retval << ArrayOf::doubleConstructor(sizeReallyRead);
                }
            } else {
                Error(_W("Problem to read data."));
            }
        } else {
            Error(_W("Invalid file identifier."));
        }
    } else {
        Error(_W("Invalid file identifier."));
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
freadBuiltinFourRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    ArrayOf param4 = argIn[3];
    ArrayOf param5;
    if (param4.isRowVectorCharacterArray()) {
        param5 = param4;
        param4 = ArrayOf::doubleConstructor(0);
    } else {
        param5 = ArrayOf::characterArrayConstructor(L"n");
    }
    ArrayOfVector modifiedArgIn;
    modifiedArgIn.push_back(param1);
    modifiedArgIn.push_back(param2);
    modifiedArgIn.push_back(param3);
    modifiedArgIn.push_back(param4);
    modifiedArgIn.push_back(param5);
    return freadBuiltinFiveRhs(nLhs, modifiedArgIn);
}
//=============================================================================
static ArrayOfVector
freadBuiltinThreeRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3 = argIn[2];
    ArrayOf param4;
    ArrayOf param5;
    if (param2.isRowVectorCharacterArray() && param3.isRowVectorCharacterArray()) {
        param5 = param3;
        param3 = param2;
        param4 = ArrayOf::doubleConstructor(0);
        param2 = ArrayOf::doubleConstructor(std::numeric_limits<double>::infinity());
    } else {
        param4 = ArrayOf::doubleConstructor(0);
        param5 = ArrayOf::characterArrayConstructor(L"n");
    }
    ArrayOfVector modifiedArgIn;
    modifiedArgIn.push_back(param1);
    modifiedArgIn.push_back(param2);
    modifiedArgIn.push_back(param3);
    modifiedArgIn.push_back(param4);
    modifiedArgIn.push_back(param5);
    return freadBuiltinFiveRhs(nLhs, modifiedArgIn);
}
//=============================================================================
static ArrayOfVector
freadBuiltinTwoRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    ArrayOf param3;
    if (param2.isRowVectorCharacterArray()) {
        param3 = param2;
        param2 = ArrayOf::doubleConstructor(std::numeric_limits<double>::infinity());
    } else {
        param3 = ArrayOf::characterArrayConstructor(L"uint8");
    }
    ArrayOf param4 = ArrayOf::doubleConstructor(0);
    ArrayOf param5 = ArrayOf::characterArrayConstructor(L"n");
    ArrayOfVector modifiedArgIn;
    modifiedArgIn.push_back(param1);
    modifiedArgIn.push_back(param2);
    modifiedArgIn.push_back(param3);
    modifiedArgIn.push_back(param4);
    modifiedArgIn.push_back(param5);
    return freadBuiltinFiveRhs(nLhs, modifiedArgIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::freadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    switch (argIn.size()) {
    case 2:
        return freadBuiltinTwoRhs(nLhs, argIn);
    case 3:
        return freadBuiltinThreeRhs(nLhs, argIn);
    case 4:
        return freadBuiltinFourRhs(nLhs, argIn);
    case 5:
        return freadBuiltinFiveRhs(nLhs, argIn);
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
