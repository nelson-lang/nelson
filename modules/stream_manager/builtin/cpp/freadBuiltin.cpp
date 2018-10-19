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
#include "freadBuiltin.hpp"
#include "Endian.hpp"
#include "Error.hpp"
#include "FileRead.hpp"
#include "FilesManager.hpp"
#include "helpers.hpp"
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
freadBuiltinFiveRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    bool bIsLittleEndian = true;
    size_t skipSize = 0;
    Class classDest = NLS_UINT8;
    int64 isize = 0;
    int32 idFile = 0;
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
    skipSize = (size_t)param4.getContentAsScalarIndex();
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
            double* dValues = (double*)param2.getReadWriteDataPointer();
            double m = dValues[0];
            double n = dValues[1];
            if (std::isinf(m)) {
                Error(ERROR_WRONG_ARGUMENT_2_INVALID_VECTOR_SIZE);
            }
            im = (int64)m;
            if (std::isinf(n)) {
                if (n > 0) {
                    isize = -1;
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_INVALID_VECTOR_SIZE);
                }
            } else {
                in = (int64)n;
                isize = (int64)(m * n);
            }
        } else {
            double dsize = (double)param2.getContentAsDoubleScalar();
            if (std::isinf(dsize)) {
                if (dsize > 0) {
                    isize = -1;
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_INVALID_VECTOR_SIZE);
                }
            } else {
                isize = (int64)dsize;
            }
        }
        FilesManager* fm = (FilesManager*)(eval->FileManager);
        int32 iValue = (int32)param1.getContentAsDoubleScalar();
        if (fm == nullptr) {
            Error(_W("Problem with file manager."));
        }
        if (fm->isOpened(iValue)) {
            File* f = fm->getFile(iValue);
            int sizeReallyRead = -1;
            ArrayOf toRead
                = FileRead(eval, f, isize, classDest, skipSize, bIsLittleEndian, sizeReallyRead);
            if (sizeReallyRead != -1) {
                if (bSizeIs2D) {
                    Dimensions dim;
                    if (isize == -1) {
                        // n is inf
                        in = sizeReallyRead / im;
                        if (sizeReallyRead % im) {
                            in++;
                        }
                        Dimensions dimL((indexType)im, (indexType)in);
                        dim = dimL;
                    } else {
                        Dimensions dimL((indexType)im, (indexType)in);
                        dim = dimL;
                    }
                    if (sizeReallyRead == im * in) {
                        toRead.reshape(dim);
                        retval.push_back(toRead);
                    } else {
                        void* ptr
                            = ArrayOf::allocateArrayOf(toRead.getDataClass(), (indexType)(im * in));
                        memcpy(ptr, toRead.getReadWriteDataPointer(), toRead.getByteSize());
                        ArrayOf Resized = ArrayOf(toRead.getDataClass(), dim, ptr);
                        retval.push_back(Resized);
                    }
                } else {
                    retval.push_back(toRead);
                }
                if (nLhs > 1) {
                    retval.push_back(ArrayOf::doubleConstructor(sizeReallyRead));
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
freadBuiltinFourRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
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
    return freadBuiltinFiveRhs(eval, nLhs, modifiedArgIn);
}
//=============================================================================
static ArrayOfVector
freadBuiltinThreeRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
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
    return freadBuiltinFiveRhs(eval, nLhs, modifiedArgIn);
}
//=============================================================================
static ArrayOfVector
freadBuiltinTwoRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
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
    return freadBuiltinFiveRhs(eval, nLhs, modifiedArgIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::freadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (nLhs > 2) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    switch (argIn.size()) {
    case 2:
        return freadBuiltinTwoRhs(eval, nLhs, argIn);
    case 3:
        return freadBuiltinThreeRhs(eval, nLhs, argIn);
    case 4:
        return freadBuiltinFourRhs(eval, nLhs, argIn);
    case 5:
        return freadBuiltinFiveRhs(eval, nLhs, argIn);
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
