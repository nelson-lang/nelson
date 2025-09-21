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
#include "crc32Builtin.hpp"
#include "NelsonCrc32.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
enum CRC_CONVERSION_TYPE
{
    FILENAME,
    STRING,
    AUTO
};
//=============================================================================
static ArrayOf
crc32Conversion(const ArrayOf& arg, CRC_CONVERSION_TYPE crcConversion)
{
    ArrayOf res;
    if (arg.isStringArray()) {
        Dimensions dimArg = arg.getDimensions();
        size_t nbArg = dimArg.getElementCount();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbArg);
        res = ArrayOf(NLS_STRING_ARRAY, dimArg, elements);
        auto* cellArg = (ArrayOf*)(arg.getDataPointer());
        for (size_t k = 0; k < nbArg; k++) {
            if (cellArg[k].isCharacterArray()) {
                std::wstring str = cellArg[k].getContentAsWideString();
                switch (crcConversion) {
                case CRC_CONVERSION_TYPE::STRING: {
                    elements[k] = ArrayOf::characterArrayConstructor(computeStringToCrc32(str));
                } break;
                case CRC_CONVERSION_TYPE::FILENAME: {
                    elements[k] = ArrayOf::characterArrayConstructor(computeFileToCrc32(str));
                } break;
                default:
                case CRC_CONVERSION_TYPE::AUTO: {
                    if (FileSystemWrapper::Path::is_regular_file(str)) {
                        elements[k] = ArrayOf::characterArrayConstructor(computeFileToCrc32(str));
                    } else {
                        elements[k] = ArrayOf::characterArrayConstructor(computeStringToCrc32(str));
                    }
                } break;
                }
            } else {
                elements[k] = ArrayOf::emptyConstructor();
            }
        }
    } else if (arg.isCellArrayOfCharacterVectors()) {
        Dimensions dimArg = arg.getDimensions();
        size_t nbArg = dimArg.getElementCount();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbArg);
        res = ArrayOf(NLS_CELL_ARRAY, dimArg, elements);
        auto* cellArg = (ArrayOf*)(arg.getDataPointer());
        for (size_t k = 0; k < nbArg; k++) {
            std::wstring str = cellArg[k].getContentAsWideString();
            switch (crcConversion) {
            case CRC_CONVERSION_TYPE::STRING: {
                elements[k] = ArrayOf::characterArrayConstructor(computeStringToCrc32(str));
            } break;
            case CRC_CONVERSION_TYPE::FILENAME: {
                elements[k] = ArrayOf::characterArrayConstructor(computeFileToCrc32(str));
            } break;
            default:
            case CRC_CONVERSION_TYPE::AUTO: {
                if (FileSystemWrapper::Path::is_regular_file(str)) {
                    elements[k] = ArrayOf::characterArrayConstructor(computeFileToCrc32(str));
                } else {
                    elements[k] = ArrayOf::characterArrayConstructor(computeStringToCrc32(str));
                }
            } break;
            }
        }
    } else if (arg.isRowVectorCharacterArray()) {
        std::wstring content = arg.getContentAsWideString();
        switch (crcConversion) {
        case CRC_CONVERSION_TYPE::FILENAME: {
            res = ArrayOf::characterArrayConstructor(computeFileToCrc32(content));
        } break;
        case CRC_CONVERSION_TYPE::STRING: {
            res = ArrayOf::characterArrayConstructor(computeStringToCrc32(content));
        } break;
        default:
        case CRC_CONVERSION_TYPE::AUTO: {
            if (FileSystemWrapper::Path::is_regular_file(content)) {
                res = ArrayOf::characterArrayConstructor(computeFileToCrc32(content));
            } else {
                res = ArrayOf::characterArrayConstructor(computeStringToCrc32(content));
            }
        } break;
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE);
    }
    return res;
}
//=============================================================================
// crc = crc32(filename)
// crc = crc32(str)
// crc = crc32(str, '-file')
// crc = crc32(str, '-string')
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crc32Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 1: {
        retval << crc32Conversion(argIn[0], CRC_CONVERSION_TYPE::AUTO);
    } break;
    case 2: {
        std::wstring param2 = argIn[1].getContentAsWideString();
        ArrayOf param1 = argIn[0];
        if (param2 == L"-file") {
            retval << crc32Conversion(argIn[0], CRC_CONVERSION_TYPE::FILENAME);
        } else if (param2 == L"-string") {
            retval << crc32Conversion(argIn[0], CRC_CONVERSION_TYPE::STRING);
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE);
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
