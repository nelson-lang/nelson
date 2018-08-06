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
#include "warningBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "Warning.hpp"
#include "WarningIds.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
formatWarningIDStateLine(std::wstring ID, WARNING_STATE state)
{
    std::wstring line;
    line = L"   ";
    if (state == WARNING_STATE::AS_ERROR) {
        line = line + L"aserror";
    }
    if (state == WARNING_STATE::DISABLED) {
        line = line + L"off";
    }
    if (state == WARNING_STATE::ENABLED) {
        line = line + L"on";
    }
    if (state == WARNING_STATE::NOT_FOUND) {
        line = line + L"notfound";
    }
    line = line + L"   " + ID + L"\n";
    return line;
}
//=============================================================================
static void
displayWarningStates(Evaluator* eval)
{
    Interface* io = eval->getInterface();
    if (io) {
        io->outputMessage(_W("By default, warnings are enabled ('on'):") + L"\n\n");
        WARNING_IDS_STATES list = getAllWarningState();
        for (size_t k = 0; k < list.IDs.size(); k++) {
            if (list.states[k] != WARNING_STATE::ENABLED) {
                io->outputMessage(formatWarningIDStateLine(list.IDs[k], list.states[k]));
            }
        }
        io->outputMessage(L"\n");
    }
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::warningBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0) {

        if (nLhs == 0 && nLhs == 0) {
            displayWarningStates(eval);
            return retval;
        }
        if (nLhs == 1) {
            WARNING_IDS_STATES list = getAllWarningState();
            stringVector fieldnames;
            fieldnames.push_back("identifier");
            fieldnames.push_back("state");
            Dimensions dims;
            dims[0] = list.IDs.size();
            dims[1] = 1;
            if (list.IDs.empty()) {
                retval.push_back(ArrayOf::emptyStructConstructor(fieldnames, dims));
            } else {
                ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(
                    NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames);
                ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
                ArrayOfVector identifiers;
                ArrayOfVector states;
                identifiers.reserve(dims[0]);
                states.reserve(dims[0]);
                for (size_t k = 0; k < list.IDs.size(); ++k) {
                    identifiers.push_back(ArrayOf::stringConstructor(list.IDs[k]));
                    switch (list.states[k]) {
                    case WARNING_STATE::AS_ERROR:
                        states.push_back(ArrayOf::stringConstructor(L"aserror"));
                        break;
                    case WARNING_STATE::DISABLED:
                        states.push_back(ArrayOf::stringConstructor(L"off"));
                        break;
                    case WARNING_STATE::ENABLED:
                        states.push_back(ArrayOf::stringConstructor(L"on"));
                        break;
                    case WARNING_STATE::NOT_FOUND:
                    default:
                        states.push_back(ArrayOf::stringConstructor(L"notfound"));
                        break;
                    }
                }
                st.setFieldAsList("identifier", identifiers);
                st.setFieldAsList("state", states);
                retval.push_back(st);
                return retval;
            }
        } else {
            Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
    }
    std::wstring msg = argIn[0].getContentAsWideString();
    Warning(msg);
    return retval;
}
//=============================================================================
