//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "warningBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Warning.hpp"
#include "WarningIds.hpp"
#include "IsWarningStruct.hpp"
#include "ErrorFunction.hpp"
#include "StringPrintf.hpp"
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
formatWarningIDStateLine(const std::wstring& ID, WARNING_STATE state)
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
    if (io != nullptr) {
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
warningStruct(const WARNING_IDS_STATES& list)
{
    ArrayOfVector retval;
    stringVector fieldnames;
    fieldnames.push_back("identifier");
    fieldnames.push_back("state");
    Dimensions dims;
    dims[0] = list.IDs.size();
    dims[1] = 1;
    if (list.IDs.empty()) {
        retval << ArrayOf::emptyStruct(fieldnames, dims);
    } else {
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fieldnames, false));
        ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        ArrayOfVector identifiers;
        ArrayOfVector states;
        identifiers.reserve(dims[0]);
        states.reserve(dims[0]);
        for (size_t k = 0; k < list.IDs.size(); ++k) {
            identifiers.push_back(ArrayOf::characterArray(list.IDs[k]));
            switch (list.states[k]) {
            case WARNING_STATE::AS_ERROR:
                states.push_back(ArrayOf::characterArray(L"aserror"));
                break;
            case WARNING_STATE::DISABLED:
                states.push_back(ArrayOf::characterArray(L"off"));
                break;
            case WARNING_STATE::ENABLED:
                states.push_back(ArrayOf::characterArray(L"on"));
                break;
            case WARNING_STATE::NOT_FOUND:
            default:
                states.push_back(ArrayOf::characterArray(L"notfound"));
                break;
            }
        }
        st.setFieldAsList("identifier", identifiers);
        st.setFieldAsList("state", states);
        retval << st;
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
        Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
warningBuiltinOneRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    if (argIn[0].isStruct()) {
        wstringVector identifiers;
        wstringVector states;
        if (IsWarningStruct(argIn[0], identifiers, states)) {
            clearWarningIdsList();
            for (const auto& state : states) {
                if (!isState(state)) {
                    Error(L"Nelson:warning:invalidStruct",
                        L"Wrong value for #%d argument: valid warning struct expected.", 1);
                }
            }
            for (size_t k = 0; k < identifiers.size(); k++) {
                setWarningId(identifiers[k], stringToState(states[k]), false);
            }
        } else {
            Error(L"Nelson:warning:invalidStruct",
                L"Wrong value for #%d argument: valid warning struct expected.", 1);
        }
    } else {
        NelsonException lastWarning = eval->getLastWarningException();
        std::wstring msg = argIn[0].asWideString();
        if (msg.empty()) {
            eval->resetLastWarningException();
            switch (nLhs) {
            case 0: {
            } break;
            case 1: {
                retval << ArrayOf::characterArray(lastWarning.message());
            } break;
            case 2: {
                retval << ArrayOf::characterArray(lastWarning.message());
                retval << ArrayOf::characterArray(lastWarning.identifier());
            } break;
            default: {
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
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
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
            } break;
            }
        } else if (isState(msg)) {
            WARNING_IDS_STATES previousList = getAllWarningState();
            setWarningId(L"all", stringToState(msg));
            switch (nLhs) {
            case 0: {
            } break;
            case 1: {
                retval = warningStruct(previousList);
            } break;
            default: {
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
            } break;
            }
        } else {
            Warning(msg);
            switch (nLhs) {
            case 0: {
            } break;
            case 1: {
                retval << ArrayOf::characterArray(lastWarning.message());
            } break;
            case 2: {
                retval << ArrayOf::characterArray(lastWarning.message());
                retval << ArrayOf::characterArray(lastWarning.identifier());
            } break;
            default: {
                Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
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
    std::wstring id = argIn[0].asWideString();
    std::wstring msg = argIn[1].asWideString();
    if (isQuery(id)) {
        // warning('query', ID): ID may be 'all' for the global default
        WARNING_STATE state = warningCheckState(msg);
        if (state == WARNING_STATE::NOT_FOUND) {
            state = WARNING_STATE::ENABLED;
        }
        switch (nLhs) {
        case 0: {
            Interface* io = eval->getInterface();
            if (io != nullptr) {
                io->outputMessage(formatWarningIDStateLine(msg, state));
            }
        } break;
        case 1: {
            wstringVector fieldnames;
            ArrayOfVector fieldvalues;
            fieldnames.push_back(L"identifier");
            fieldnames.push_back(L"state");
            fieldvalues.push_back(ArrayOf::characterArray(msg));
            fieldvalues.push_back(ArrayOf::characterArray(stateToString(state)));
            retval << ArrayOf::structArray(fieldnames, fieldvalues);
        } break;
        default: {
            Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
        } break;
        }
    } else if (isState(id)) {
        WARNING_STATE state = stringToState(id);
        setWarningId(msg, state);
        switch (nLhs) {
        case 0: {
        } break;
        case 1: {
            wstringVector fieldnames;
            ArrayOfVector fieldvalues;
            fieldnames.push_back(L"identifier");
            fieldnames.push_back(L"state");
            fieldvalues.push_back(ArrayOf::characterArray(msg));
            fieldvalues.push_back(ArrayOf::characterArray(id));
            retval << ArrayOf::structArray(fieldnames, fieldvalues);
        } break;
        default: {
            Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
        } break;
        }
    } else {
        NelsonException lastWarning = eval->getLastWarningException();
        RaiseWarning(id, msg);
        switch (nLhs) {
        case 0: {
        } break;
        case 1: {
            retval << ArrayOf::characterArray(lastWarning.message());
        } break;
        case 2: {
            retval << ArrayOf::characterArray(lastWarning.message());
            retval << ArrayOf::characterArray(lastWarning.identifier());
        } break;
        default: {
            Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
        } break;
        }
    }
    return retval;
}
//=============================================================================
// warning(IDENTIFIER, FORMAT, A1, A2, ...): printf-style, like error(). The
// first argument may also be a plain format string (no identifier).
static ArrayOfVector
warningBuiltinFormatRhs(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    NelsonException lastWarning = eval->getLastWarningException();
    ErrorParseResult parsed = ParseErrorFunctionArguments(argIn);
    if (parsed.needsFormatting) {
        std::wstring message;
        std::wstring formatError;
        if (!StringPrintf(message, formatError, eval, parsed.formatArguments)) {
            Error(L"Nelson:warning:formatFailed", formatError);
        }
        parsed.exception.setMessage(message);
    }
    RaiseWarning(parsed.exception.identifier(), parsed.exception.message());
    switch (nLhs) {
    case 0: {
    } break;
    case 1: {
        retval << ArrayOf::characterArray(lastWarning.message());
    } break;
    case 2: {
        retval << ArrayOf::characterArray(lastWarning.message());
        retval << ArrayOf::characterArray(lastWarning.identifier());
    } break;
    default: {
        Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
    } break;
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
        return warningBuiltinFormatRhs(eval, nLhs, argIn);
    } break;
    }
    return retval;
}
//=============================================================================
