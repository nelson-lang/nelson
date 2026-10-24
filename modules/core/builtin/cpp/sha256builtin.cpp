//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "sha256Builtin.hpp"
#include "NelsonSHA256.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
enum SHA256_CONVERSION_TYPE
{
    FILENAME,
    STRING,
    AUTO
};
//=============================================================================
static ArrayOf
sha256Conversion(const ArrayOf& arg, SHA256_CONVERSION_TYPE sha256Conversion)
{
    ArrayOf res;
    if (arg.isStringArray()) {
        Dimensions dimArg = arg.dimensions();
        size_t nbArg = dimArg.elementCount();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbArg);
        res = ArrayOf(NLS_STRING_ARRAY, dimArg, elements);
        auto* cellArg = (ArrayOf*)(arg.dataPointer());
        for (size_t k = 0; k < nbArg; k++) {
            if (cellArg[k].isCharacterArray()) {
                std::wstring str = cellArg[k].asWideString();
                switch (sha256Conversion) {
                case SHA256_CONVERSION_TYPE::STRING: {
                    elements[k] = ArrayOf::characterArray(computeStringToSHA256(str));
                } break;
                case SHA256_CONVERSION_TYPE::FILENAME: {
                    elements[k] = ArrayOf::characterArray(computeFileToSHA256(str));
                } break;
                default:
                case SHA256_CONVERSION_TYPE::AUTO: {
                    if (FileSystemWrapper::Path::is_regular_file(str)) {
                        elements[k] = ArrayOf::characterArray(computeFileToSHA256(str));
                    } else {
                        elements[k] = ArrayOf::characterArray(computeStringToSHA256(str));
                    }
                } break;
                }
            } else {
                elements[k] = ArrayOf::empty();
            }
        }
    } else if (arg.isCellArrayOfCharacterVectors()) {
        Dimensions dimArg = arg.dimensions();
        size_t nbArg = dimArg.elementCount();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbArg);
        res = ArrayOf(NLS_CELL_ARRAY, dimArg, elements);
        auto* cellArg = (ArrayOf*)(arg.dataPointer());
        for (size_t k = 0; k < nbArg; k++) {
            std::wstring str = cellArg[k].asWideString();
            switch (sha256Conversion) {
            case SHA256_CONVERSION_TYPE::STRING: {
                elements[k] = ArrayOf::characterArray(computeStringToSHA256(str));
            } break;
            case SHA256_CONVERSION_TYPE::FILENAME: {
                elements[k] = ArrayOf::characterArray(computeFileToSHA256(str));
            } break;
            default:
            case SHA256_CONVERSION_TYPE::AUTO: {
                if (FileSystemWrapper::Path::is_regular_file(str)) {
                    elements[k] = ArrayOf::characterArray(computeFileToSHA256(str));
                } else {
                    elements[k] = ArrayOf::characterArray(computeStringToSHA256(str));
                }
            } break;
            }
        }
    } else if (arg.isRowVectorCharacterArray()) {
        std::wstring content = arg.asWideString();
        switch (sha256Conversion) {
        case SHA256_CONVERSION_TYPE::FILENAME: {
            res = ArrayOf::characterArray(computeFileToSHA256(content));
        } break;
        case SHA256_CONVERSION_TYPE::STRING: {
            res = ArrayOf::characterArray(computeStringToSHA256(content));
        } break;
        default:
        case SHA256_CONVERSION_TYPE::AUTO: {
            if (FileSystemWrapper::Path::is_regular_file(content)) {
                res = ArrayOf::characterArray(computeFileToSHA256(content));
            } else {
                res = ArrayOf::characterArray(computeStringToSHA256(content));
            }
        } break;
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string array, cell of strings or character vector");
    }
    return res;
}
//=============================================================================
// hash256 = sha256(filename)
// hash256 = sha256(str)
// hash256 = sha256(str, '-file')
// hash256 = sha256(str, '-string')
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::sha256Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    switch (argIn.size()) {
    case 1: {
        retval << sha256Conversion(argIn[0], SHA256_CONVERSION_TYPE::AUTO);
    } break;
    case 2: {
        std::wstring param2 = argIn[1].asWideString();
        ArrayOf param1 = argIn[0];
        if (param2 == L"-file") {
            retval << sha256Conversion(argIn[0], SHA256_CONVERSION_TYPE::FILENAME);
        } else if (param2 == L"-string") {
            retval << sha256Conversion(argIn[0], SHA256_CONVERSION_TYPE::STRING);
        } else {
            Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
        }
    } break;
    default: {
    } break;
    }
    return retval;
}
//=============================================================================
