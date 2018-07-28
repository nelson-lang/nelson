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
#include "formatBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::formatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0) {
        if (nLhs > 1) {
            Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (nLhs == 1) {
            switch (eval->getCurrentOutputFormatDisplay()) {
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
                Error(eval, L"Unexpected format.");
            } break;
            }
        } else {
            eval->setCurrentOutputFormatDisplay(NLS_FORMAT_SHORT);
        }
        return retval;
    }
    if (argIn.size() == 1) {
        if (argIn[0].isColonVectorCharacterArray()) {
            std::wstring str = argIn[0].getContentAsWideString();
            if (str.compare(L"get") == 0) {
                if (nLhs > 1) {
                    Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                switch (eval->getCurrentOutputFormatDisplay()) {
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
                    Error(eval, L"Unexpected format.");
                } break;
                }
            } else {
                if (nLhs != 0) {
                    Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                if (str.compare(L"short") == 0) {
                    eval->setCurrentOutputFormatDisplay(NLS_FORMAT_SHORT);
                } else if (str.compare(L"long") == 0) {
                    eval->setCurrentOutputFormatDisplay(NLS_FORMAT_LONG);
                } else if (str.compare(L"shortE") == 0) {
                    eval->setCurrentOutputFormatDisplay(NLS_FORMAT_SHORTE);
                } else if (str.compare(L"longE") == 0) {
                    eval->setCurrentOutputFormatDisplay(NLS_FORMAT_LONGE);
                } else if (str.compare(L"hex") == 0) {
                    eval->setCurrentOutputFormatDisplay(NLS_FORMAT_HEX);
                } else {
                    Error(eval, _W("unexpected format."));
                }
            }
        } else {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
