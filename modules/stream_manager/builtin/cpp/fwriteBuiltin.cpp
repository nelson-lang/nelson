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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FileWrite.hpp"
#include "FilesManager.hpp"
#include "helpers.hpp"
#include "NelsonConfiguration.hpp"
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
    std::wstring arg = param5.asWideString();
    if ((arg == L"n") || (arg == L"native")) {
        bIsLittleEndian = isLittleEndianFormat();
    } else if ((arg == L"b") || (arg == L"ieee-be")) {
        bIsLittleEndian = false;
    } else if ((arg == L"l") || (arg == L"ieee-le")) {
        bIsLittleEndian = true;
    } else {
        Error(L"Nelson:stream_manager:invalidMachineFormat", L"Wrong value for machine format.");
    }
    ArrayOf param4 = argIn[3];
    auto skipSize = static_cast<size_t>(param4.asScalarIndex());
    ArrayOf param3 = argIn[2];
    NelsonType classDest = NLS_UINT8;
    if (param3.isRowVectorCharacterArray()) {
        std::wstring precisionStr = param3.asWideString();
        bool bOK = false;
        classDest = precisionFromString(precisionStr, bOK);
        if (!bOK) {
            Error(L"Nelson:stream_manager:unsupportedPrecision",
                L"Wrong value for #%d argument: not supported precision.", 3);
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (3), L"string");
    }
    ArrayOf param1 = argIn[0];
    if (param1.isDoubleType()) {
        ArrayOf param2 = argIn[1];
        if (param2.isReferenceType()) {
            Error(L"Nelson:fwrite:cannotWriteHandle", L"Cannot write references type.");
        }
        if (param2.isSparse()) {
            Error(L"Nelson:fwrite:cannotWriteSparse", L"Cannot write sparse type.");
        }
        auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
        if (fm == nullptr) {
            Error(L"Nelson:stream_manager:fileManagerNotAvailable", L"Problem with file manager.");
            return retval;
        }
        auto iValue = static_cast<int32>(param1.asDoubleScalar());
        if (fm->isOpened(iValue)) {
            File* f = fm->getFile(iValue);
            int written = -1;
            int writtenBytes = -1;
            FWRITE_ERROR_TYPE fwriteError
                = FileWrite(f, param2, classDest, skipSize, bIsLittleEndian, written, writtenBytes);
            switch (fwriteError) {
            case FWRITE_NO_ERROR: {
                if (nLhs > 0) {
                    retval << ArrayOf::doubleScalar((double)written);
                }
                if (nLhs > 1) {
                    retval << ArrayOf::doubleScalar((double)writtenBytes);
                }
            } break;
            case FWRITE_DATA_TYPE_NOT_SUPPORTED: {
                Error(L"Nelson:fwrite:typeNotSupported", L"Type not supported.");
            } break;
            case FWRITE_ALLOCATION_MEMORY: {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            } break;
            case FWRITE_FILE_DESTINATION_NOT_SUPPORTED:
            case FWRITE_INVALID_FILE: {
                Error(L"Nelson:stream_manager:invalidFileIdentifier", L"Invalid file identifier.");
            } break;
            case FWRITE_ENDIAN_CONVERSION_NOT_SUPPORTED: {
                Error(L"Nelson:fwrite:endianConversionNotSupported",
                    L"Endian conversion not supported for this file identifier.");
            } break;
            case FWRITE_ERROR_ENCODING: {
                Error(L"Nelson:fwrite:encodingConversionNotSupported",
                    L"encoding conversion not supported for this file identifier.");
            } break;
            }
        } else {
            Error(L"Nelson:stream_manager:invalidFileIdentifierArgument",
                L"Wrong value for #%d argument: a valid file ID expected.", 1);
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"double");
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
            modifiedArgIn.push_back(ArrayOf::doubleScalar(0.));
            modifiedArgIn.push_back(argIn[3]);
        } else {
            modifiedArgIn.push_back(argIn[3]);
            modifiedArgIn.push_back(ArrayOf::characterArray(L"n"));
        }
    } else {
        modifiedArgIn.push_back(argIn[3]);
        modifiedArgIn.push_back(argIn[2]);
        modifiedArgIn.push_back(ArrayOf::characterArray(L"n"));
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
    modifiedArgIn.push_back(ArrayOf::doubleScalar(0.));
    modifiedArgIn.push_back(ArrayOf::characterArray(L"n"));
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
        modifiedArgIn.push_back(ArrayOf::characterArray(L"char"));
    } else {
        modifiedArgIn.push_back(ArrayOf::characterArray(L"uint8"));
    }
    modifiedArgIn.push_back(ArrayOf::doubleScalar(0.));
    modifiedArgIn.push_back(ArrayOf::characterArray(L"n"));
    return fwriteBuiltinFiveRhs(nLhs, modifiedArgIn);
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::fwriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 5);
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
    default:
        break;
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
