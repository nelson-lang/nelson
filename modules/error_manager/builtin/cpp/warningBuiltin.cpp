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
#include "Warning.hpp"
#include "WarningIds.hpp"
#include "IsWarningStruct.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isQuery(const std::wstring& state)
{
    return state == L"query";
}
//=============================================================================
static bool
isState(const std::wstring& state)
{
    if (state == L"aserror") {
        return true;
    }
    if (state == L"off") {
        return true;
    }
    if (state == L"on") {
        return true;
    }
    return false;
}
//=============================================================================
static WARNING_STATE
stringToState(const std::wstring& str)
{
    if (str == L"aserror") {
        return WARNING_STATE::AS_ERROR;
    }
    if (str == L"off") {
        return WARNING_STATE::DISABLED;
    }
    if (str == L"on") {
        return WARNING_STATE::ENABLED;
    }
    return WARNING_STATE::NOT_FOUND;
}
//=============================================================================
static std::wstring
stateToString(WARNING_STATE state)
{
    std::wstring str = L"on";
    switch (state) {
    case WARNING_STATE::AS_ERROR:
        return L"aserror";
    case WARNING_STATE::DISABLED:
        return L"off";
    case WARNING_STATE::ENABLED:
        return L"on";
    case WARNING_STATE::NOT_FOUND:
        return L"notfound";
    }
    return str;
}
//=============================================================================
static std::wstring
formatWarningIDStateLine(std::wstring ID, WARNING_STATE state)
{
    std::wstring line;
    line = L"   ";
    line = line + stateToString(state);
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
static ArrayOfVector
warningStruct(WARNING_IDS_STATES list)
{
    ArrayOfVector retval;
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
            identifiers.push_back(ArrayOf::characterArrayConstructor(list.IDs[k]));
            switch (list.states[k]) {
            case WARNING_STATE::AS_ERROR:
                states.push_back(ArrayOf::characterArrayConstructor(L"aserror"));
                break;
            case WARNING_STATE::DISABLED:
                states.push_back(ArrayOf::characterArrayConstructor(L"off"));
                break;
            case WARNING_STATE::ENABLED:
                states.push_back(ArrayOf::characterArrayConstructor(L"on"));
                break;
            case WARNING_STATE::NOT_FOUND:
            default:
                states.push_back(ArrayOf::characterArrayConstructor(L"notfound"));
                break;
            }
        }
        st.setFieldAsList("identifier", identifiers);
        st.setFieldAsList("state", states);
        retval.push_back(st);
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
warningBuiltinNoRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;

    switch (nLhs) {
    case 0: {
        displayWarningStates(eval);
    } break;
    case 1: {
        retval = warningStruct(getAllWarningState());
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
warningBuiltinOneRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn[0].isStruct()) {
        wstringVector identifiers;
        wstringVector states;
        if (IsWarningStruct(argIn[0], identifiers, states)) {
            clearWarningIdsList();
            for (size_t k = 0; k < states.size(); k++) {
                if (!isState(states[k])) {
                    Error(_W("Wrong value for #1 argument: valid warning struct expected."));
                }
            }
            for (size_t k = 0; k < identifiers.size(); k++) {
                setWarningId(identifiers[k], stringToState(states[k]), false);
            }
        } else {
            Error(_W("Wrong value for #1 argument: valid warning struct expected."));
        }
    } else {
        Exception lastWarning = eval->getLastWarningException();
        std::wstring msg = argIn[0].getContentAsWideString();
        if (msg == L"") {
            eval->resetLastWarningException();
            switch (nLhs) {
            case 0: {
            } break;
            case 1: {
                retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getMessage()));
            } break;
            case 2: {
                retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getMessage()));
                retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getIdentifier()));
            }
            default: {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            } break;
            }
        } else if (isQuery(msg)) {
            switch (nLhs) {
            case 0: {
                displayWarningStates(eval);
            } break;
            case 1: {
                retval = warningStruct(getAllWarningState());
            } break;
            default: {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            } break;
            }
        } else if (isState(msg)) {
            WARNING_IDS_STATES previousList = getAllWarningState();
            setWarningId(L"all", stringToState(msg));
            switch (nLhs) {
            case 0: {
                // NOTHING TO DO
            } break;
            case 1: {
                retval = warningStruct(previousList);
            } break;
            default: {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            } break;
            }
        } else {
            Warning(msg);
            switch (nLhs) {
            case 0: {
                // NOTHING TO DO
            } break;
            case 1: {
                retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getMessage()));
            } break;
            case 2: {
                retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getMessage()));
                retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getIdentifier()));
            } break;
            default: {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            } break;
            }
        }
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
warningBuiltinTwoRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    std::wstring id = argIn[0].getContentAsWideString();
    std::wstring msg = argIn[1].getContentAsWideString();
    if (isQuery(id)) {
        Error(_W("warning('query') does not require an second argument."));
    } else if (isState(id)) {
        WARNING_STATE state = stringToState(id);
        setWarningId(msg, state);
        switch (nLhs) {
        case 0: {
            // NOTHING TO DO
        } break;
        case 1: {
            wstringVector fieldnames;
            ArrayOfVector fieldvalues;
            fieldnames.push_back(L"identifier");
            fieldnames.push_back(L"state");
            fieldvalues.push_back(ArrayOf::characterArrayConstructor(msg));
            fieldvalues.push_back(ArrayOf::characterArrayConstructor(id));
            retval.push_back(ArrayOf::structConstructor(fieldnames, fieldvalues));
        } break;
        default: {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        } break;
        }
    } else {
        Exception lastWarning = eval->getLastWarningException();
        Warning(id, msg);
        switch (nLhs) {
        case 0: {
            // NOTHING TO DO
        } break;
        case 1: {
            retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getMessage()));
        } break;
        case 2: {
            retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getMessage()));
            retval.push_back(ArrayOf::characterArrayConstructor(lastWarning.getIdentifier()));
        } break;
        default: {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        } break;
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::warningBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    switch (argIn.size()) {
    case 0: {
        return warningBuiltinNoRhs(eval, nLhs, argIn);
    } break;
    case 1: {
        return warningBuiltinOneRhs(eval, nLhs, argIn);
    } break;
    case 2: {
        return warningBuiltinTwoRhs(eval, nLhs, argIn);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
