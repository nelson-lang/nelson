//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fwriteBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileWrite.hpp"
#include "FilesManager.hpp"
#include "helpers.hpp"
#include "NelsonConfiguration.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// fwrite(fid, data)
// fwrite (fid, data, precision)
// fwrite (fid, data, precision, skip)
// fwrite(fid, data, precision, arch)
// fwrite(fid, data, precision, skip, arch)
//=============================================================================
static ArrayOfVector
fwriteBuiltinFiveRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf param5 = argIn[4];
    bool bIsLittleEndian = true;
    std::wstring arg = param5.getContentAsWideString();
    if ((arg == L"n") || (arg == L"native")) {
        bIsLittleEndian = isLittleEndianFormat();
    } else if ((arg == L"b") || (arg == L"ieee-be")) {
        bIsLittleEndian = false;
    } else if ((arg == L"l") || (arg == L"ieee-le")) {
        bIsLittleEndian = true;
    } else {
        raiseError(L"Nelson:stream_manager:ERROR_WRONG_VALUE_FOR_MACHINE_FORMAT",
            ERROR_WRONG_VALUE_FOR_MACHINE_FORMAT);
    }
    ArrayOf param4 = argIn[3];
    auto skipSize = static_cast<size_t>(param4.getContentAsScalarIndex());
    ArrayOf param3 = argIn[2];
    NelsonType classDest = NLS_UINT8;
    if (param3.isRowVectorCharacterArray()) {
        std::wstring precisionStr = param3.getContentAsWideString();
        bool bOK = false;
        classDest = precisionFromString(precisionStr, bOK);
        if (!bOK) {
            raiseError(L"Nelson:stream_manager:ERROR_WRONG_VALUE_ARG3_NOT_SUPPORTED_PRECISION",
                ERROR_WRONG_VALUE_ARG3_NOT_SUPPORTED_PRECISION);
        }
    } else {
        raiseError2(L"nelson:validators:mustBeType", 3, NLS_STRING_ARRAY_STR);
    }
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        ArrayOf param2 = argIn[1];
        if (param2.isReferenceType()) {
            raiseError(L"Nelson:stream_manager:ERROR_CANNOT_WRITE_REFERENCES_TYPE",
                ERROR_CANNOT_WRITE_REFERENCES_TYPE);
        }
        if (param2.isSparse()) {
            raiseError(L"Nelson:stream_manager:ERROR_CANNOT_WRITE_SPARSE_TYPE",
                ERROR_CANNOT_WRITE_SPARSE_TYPE);
        }
        auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
        if (fm == nullptr) {
            raiseError2(L"nelson:io:fileManagerError");
            return retval;
        }
        auto iValue = static_cast<int32>(param1.getContentAsDoubleScalar());
        if (fm->isOpened(iValue)) {
            File* f = fm->getFile(iValue);
            int written = -1;
            int writtenBytes = -1;
            FWRITE_ERROR_TYPE fwriteError
                = FileWrite(f, param2, classDest, skipSize, bIsLittleEndian, written, writtenBytes);
            switch (fwriteError) {
            case FWRITE_NO_ERROR: {
                if (nLhs > 0) {
                    retval << ArrayOf::doubleConstructor((double)written);
                }
                if (nLhs > 1) {
                    retval << ArrayOf::doubleConstructor((double)writtenBytes);
                }
            } break;
            case FWRITE_DATA_TYPE_NOT_SUPPORTED: {
                raiseError2(L"nelson:runtime:typeNotSupported");
            } break;
            case FWRITE_ALLOCATION_MEMORY: {
                raiseError2(L"nelson:runtime:outOfMemory");
            } break;
            case FWRITE_FILE_DESTINATION_NOT_SUPPORTED:
            case FWRITE_INVALID_FILE: {
                raiseError(L"Nelson:stream_manager:ERROR_INVALID_FILE_IDENTIFIER",
                    ERROR_INVALID_FILE_IDENTIFIER);
            } break;
            case FWRITE_ENDIAN_CONVERSION_NOT_SUPPORTED: {
                raiseError(
                    L"Nelson:stream_manager:ERROR_ENDIAN_CONVERSION_NOT_SUPPORTED_FOR_FILEID",
                    ERROR_ENDIAN_CONVERSION_NOT_SUPPORTED_FOR_FILEID);
            } break;
            case FWRITE_ERROR_ENCODING: {
                raiseError(
                    L"Nelson:stream_manager:ERROR_ENCODING_CONVERSION_NOT_SUPPORTED_FOR_FILEID",
                    ERROR_ENCODING_CONVERSION_NOT_SUPPORTED_FOR_FILEID);
            } break;
            }
        } else {
            raiseError(L"Nelson:stream_manager:ERROR_INVALID_FILE_ID_EXPECTED",
                ERROR_INVALID_FILE_ID_EXPECTED);
        }
    } else {
        raiseError2(L"nelson:validators:mustBeType", 1, NLS_DOUBLE_STR);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
fwriteBuiltinFourRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector modifiedArgIn;
    modifiedArgIn.push_back(argIn[0]);
    modifiedArgIn.push_back(argIn[1]);
    if (argIn[2].isRowVectorCharacterArray()) {
        modifiedArgIn.push_back(argIn[2]);
        if (argIn[3].isRowVectorCharacterArray()) {
            modifiedArgIn.push_back(ArrayOf::doubleConstructor(0.));
            modifiedArgIn.push_back(argIn[3]);
        } else {
            modifiedArgIn.push_back(argIn[3]);
            modifiedArgIn.push_back(ArrayOf::characterArrayConstructor(L"n"));
        }
    } else {
        modifiedArgIn.push_back(argIn[3]);
        modifiedArgIn.push_back(argIn[2]);
        modifiedArgIn.push_back(ArrayOf::characterArrayConstructor(L"n"));
    }
    return fwriteBuiltinFiveRhs(nLhs, modifiedArgIn);
}
//=============================================================================
static ArrayOfVector
fwriteBuiltinThreeRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector modifiedArgIn;
    modifiedArgIn.push_back(argIn[0]);
    modifiedArgIn.push_back(argIn[1]);
    modifiedArgIn.push_back(argIn[2]);
    modifiedArgIn.push_back(ArrayOf::doubleConstructor(0.));
    modifiedArgIn.push_back(ArrayOf::characterArrayConstructor(L"n"));
    return fwriteBuiltinFiveRhs(nLhs, modifiedArgIn);
}
//=============================================================================
static ArrayOfVector
fwriteBuiltinTwoRhs(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector modifiedArgIn;
    modifiedArgIn.push_back(argIn[0]);
    modifiedArgIn.push_back(argIn[1]);
    if (argIn[1].isCharacterArray()) {
        modifiedArgIn.push_back(ArrayOf::characterArrayConstructor(L"char"));
    } else {
        modifiedArgIn.push_back(ArrayOf::characterArrayConstructor(L"uint8"));
    }
    modifiedArgIn.push_back(ArrayOf::doubleConstructor(0.));
    modifiedArgIn.push_back(ArrayOf::characterArrayConstructor(L"n"));
    return fwriteBuiltinFiveRhs(nLhs, modifiedArgIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fwriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    switch (argIn.size()) {
    case 2:
        return fwriteBuiltinTwoRhs(nLhs, argIn);
    case 3:
        return fwriteBuiltinThreeRhs(nLhs, argIn);
    case 4:
        return fwriteBuiltinFourRhs(nLhs, argIn);
    case 5:
        return fwriteBuiltinFiveRhs(nLhs, argIn);
    default: {
        if (argIn.size() < 2) {
            raiseError2(L"nelson:arguments:tooFewInputs");
        } else {
            raiseError2(L"nelson:arguments:tooManyInputs");
        }
    } break;
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
