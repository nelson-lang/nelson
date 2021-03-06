//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem.hpp>
#include "sha256Builtin.hpp"
#include "NelsonSHA256.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "IsCellOfStrings.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
typedef enum
{
    FILENAME,
    STRING,
    AUTO
} SHA256_CONVERSION_TYPE;
//=============================================================================
static bool
isFile(const std::wstring& filename)
{
    bool bIsFile;
    try {
        bIsFile = boost::filesystem::exists(filename) && !boost::filesystem::is_directory(filename);
    } catch (const boost::filesystem::filesystem_error&) {
        bIsFile = false;
    }
    return bIsFile;
}
//=============================================================================
static ArrayOf
sha256Conversion(const ArrayOf& arg, SHA256_CONVERSION_TYPE sha256Conversion)
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
                switch (sha256Conversion) {
                case SHA256_CONVERSION_TYPE::STRING: {
                    elements[k] = ArrayOf::characterArrayConstructor(computeStringToSHA256(str));
                } break;
                case SHA256_CONVERSION_TYPE::FILENAME: {
                    elements[k] = ArrayOf::characterArrayConstructor(computeFileToSHA256(str));
                } break;
                default:
                case SHA256_CONVERSION_TYPE::AUTO: {
                    if (isFile(str)) {
                        elements[k] = ArrayOf::characterArrayConstructor(computeFileToSHA256(str));
                    } else {
                        elements[k]
                            = ArrayOf::characterArrayConstructor(computeStringToSHA256(str));
                    }
                } break;
                }
            } else {
                elements[k] = ArrayOf::emptyConstructor();
            }
        }
    } else if (IsCellOfString(arg)) {
        Dimensions dimArg = arg.getDimensions();
        size_t nbArg = dimArg.getElementCount();
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbArg);
        res = ArrayOf(NLS_CELL_ARRAY, dimArg, elements);
        auto* cellArg = (ArrayOf*)(arg.getDataPointer());
        for (size_t k = 0; k < nbArg; k++) {
            std::wstring str = cellArg[k].getContentAsWideString();
            switch (sha256Conversion) {
            case SHA256_CONVERSION_TYPE::STRING: {
                elements[k] = ArrayOf::characterArrayConstructor(computeStringToSHA256(str));
            } break;
            case SHA256_CONVERSION_TYPE::FILENAME: {
                elements[k] = ArrayOf::characterArrayConstructor(computeFileToSHA256(str));
            } break;
            default:
            case SHA256_CONVERSION_TYPE::AUTO: {
                if (isFile(str)) {
                    elements[k] = ArrayOf::characterArrayConstructor(computeFileToSHA256(str));
                } else {
                    elements[k] = ArrayOf::characterArrayConstructor(computeStringToSHA256(str));
                }
            } break;
            }
        }
    } else if (arg.isRowVectorCharacterArray()) {
        std::wstring content = arg.getContentAsWideString();
        switch (sha256Conversion) {
        case SHA256_CONVERSION_TYPE::FILENAME: {
            res = ArrayOf::characterArrayConstructor(computeFileToSHA256(content));
        } break;
        case SHA256_CONVERSION_TYPE::STRING: {
            res = ArrayOf::characterArrayConstructor(computeStringToSHA256(content));
        } break;
        default:
        case SHA256_CONVERSION_TYPE::AUTO: {
            if (isFile(content)) {
                res = ArrayOf::characterArrayConstructor(computeFileToSHA256(content));
            } else {
                res = ArrayOf::characterArrayConstructor(computeStringToSHA256(content));
            }
        } break;
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE);
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
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    switch (argIn.size()) {
    case 1: {
        retval << sha256Conversion(argIn[0], SHA256_CONVERSION_TYPE::AUTO);
    } break;
    case 2: {
        std::wstring param2 = argIn[1].getContentAsWideString();
        ArrayOf param1 = argIn[0];
        if (param2 == L"-file") {
            retval << sha256Conversion(argIn[0], SHA256_CONVERSION_TYPE::FILENAME);
        } else if (param2 == L"-string") {
            retval << sha256Conversion(argIn[0], SHA256_CONVERSION_TYPE::STRING);
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
