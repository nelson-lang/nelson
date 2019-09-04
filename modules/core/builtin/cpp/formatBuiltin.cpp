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
#include "formatBuiltin.hpp"
#include "Error.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::formatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (nLhs == 1) {
            switch (NelsonConfiguration::getInstance()->getOutputFormatDisplay()) {
            case NLS_FORMAT_SHORT: {
                retval.push_back(ArrayOf::characterArrayConstructor(L"short"));
            } break;
            case NLS_FORMAT_LONG: {
                retval.push_back(ArrayOf::characterArrayConstructor(L"long"));
            } break;
            case NLS_FORMAT_SHORTE: {
                retval.push_back(ArrayOf::characterArrayConstructor(L"shortE"));
            } break;
            case NLS_FORMAT_LONGE: {
                retval.push_back(ArrayOf::characterArrayConstructor(L"longE"));
            } break;
            case NLS_FORMAT_HEX: {
                retval.push_back(ArrayOf::characterArrayConstructor(L"hex"));
            } break;
            default: {
                Error(L"Unexpected format.");
            } break;
            }
        } else {
            NelsonConfiguration::getInstance()->setOutputFormatDisplay(NLS_FORMAT_SHORT);
        }
        return retval;
    }
    if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring str = argIn[0].getContentAsWideString();
            if (str == L"get") {
                if (nLhs > 1) {
                    Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                switch (NelsonConfiguration::getInstance()->getOutputFormatDisplay()) {
                case NLS_FORMAT_SHORT: {
                    retval.push_back(ArrayOf::characterArrayConstructor(L"short"));
                } break;
                case NLS_FORMAT_LONG: {
                    retval.push_back(ArrayOf::characterArrayConstructor(L"long"));
                } break;
                case NLS_FORMAT_SHORTE: {
                    retval.push_back(ArrayOf::characterArrayConstructor(L"shortE"));
                } break;
                case NLS_FORMAT_LONGE: {
                    retval.push_back(ArrayOf::characterArrayConstructor(L"longE"));
                } break;
                case NLS_FORMAT_HEX: {
                    retval.push_back(ArrayOf::characterArrayConstructor(L"hex"));
                } break;
                default: {
                    Error(L"Unexpected format.");
                } break;
                }
            } else {
                if (nLhs != 0) {
                    Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                if (str == L"short") {
                    NelsonConfiguration::getInstance()->setOutputFormatDisplay(NLS_FORMAT_SHORT);
                } else if (str == L"long") {
                    NelsonConfiguration::getInstance()->setOutputFormatDisplay(NLS_FORMAT_LONG);
                } else if (str == L"shortE") {
                    NelsonConfiguration::getInstance()->setOutputFormatDisplay(NLS_FORMAT_SHORTE);
                } else if (str == L"longE") {
                    NelsonConfiguration::getInstance()->setOutputFormatDisplay(NLS_FORMAT_LONGE);
                } else if (str == L"hex") {
                    NelsonConfiguration::getInstance()->setOutputFormatDisplay(NLS_FORMAT_HEX);
                } else {
                    Error(_W("unexpected format."));
                }
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
