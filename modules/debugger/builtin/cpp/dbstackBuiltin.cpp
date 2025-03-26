//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "dbstackBuiltin.hpp"
#include "DebugStack.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PositionScript.hpp"
#include "CallStack.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
checkArgument(Evaluator* eval, const ArrayOf& arg, bool& withCompleteNames, int& nbOmits,
    bool& isCompleteNames, bool& isNbOmits)
{
    if (arg.isRowVectorCharacterArray()) {
        std::wstring str = arg.getContentAsWideString();
        if (str != L"-completenames") {
            Error(_W("'-completenames' expected."));
        }
        withCompleteNames = true;
        isCompleteNames = true;
    } else if (arg.isScalar()) {
        ArrayOf param1 = arg;
        double value = 0;
        if (param1.isIntegerType()) {
            param1.promoteType(NLS_DOUBLE);
        }
        switch (param1.getDataClass()) {
        case NLS_DOUBLE: {
            value = param1.getContentAsDoubleScalar();
        } break;
        case NLS_SINGLE: {
            value = static_cast<double>(param1.getContentAsSingleScalar());
        } break;
        default: {
            Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
        } break;
        }
        int intValue = static_cast<int>(value);
        if (static_cast<double>(intValue) != value) {
            Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
        }
        nbOmits = intValue + 1;
        isNbOmits = true;
    } else {
        Error(_W("'-completenames' expected or scalar integer value required."));
    }
}
//=============================================================================
static std::wstring
shortName(const std::wstring& filename)
{
    FileSystemWrapper::Path p(filename);
    return p.filename().generic_wstring();
}
//=============================================================================
static ArrayOf
dbstackAsStruct(stackTrace positions, bool withCompleteNames)
{
    stringVector fieldnames;
    fieldnames.reserve(3);
    ArrayOf st;
    fieldnames.push_back("file");
    fieldnames.push_back("name");
    fieldnames.push_back("line");
    Dimensions dims;
    dims[0] = positions.size();
    dims[1] = 1;
    if (positions.empty()) {
        st = ArrayOf::emptyStructConstructor(fieldnames, dims);
    } else {
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false));
        st = ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fieldnames);
        ArrayOfVector file;
        ArrayOfVector name;
        ArrayOfVector line;
        file.reserve(positions.size());
        name.reserve(positions.size());
        line.reserve(positions.size());
        for (auto& position : positions) {
            std::wstring filename = position.getFilename();
            if (!withCompleteNames) {
                filename = shortName(position.getFilename());
            }
            file.push_back(ArrayOf::characterArrayConstructor(filename));
            std::wstring functionName = position.getFunctionName();
            name.push_back(ArrayOf::characterArrayConstructor(functionName));
            if (position.getLine() == 0) {
                line.push_back(ArrayOf::emptyConstructor(0, 1));
            } else {
                line.push_back(ArrayOf::doubleConstructor((double)position.getLine()));
            }
        }
        st.setFieldAsList("file", file);
        st.setFieldAsList("name", name);
        st.setFieldAsList("line", line);
    }
    return st;
}
//=============================================================================
void
dbstackPrint(Interface* io, stackTrace positions, bool withCompleteNames)
{
    for (auto& position : positions) {
        std::wstring message;
        std::wstring filename = position.getFilename();
        if (!withCompleteNames) {
            filename = shortName(position.getFilename());
        }
        if (position.getLine() == 0) {
            if (!filename.empty()) {
                message = std::wstring(L"In ") + filename + L"\n";
            }
        } else {
            if (!position.getFunctionName().empty()) {
                message = std::wstring(L"In ") + filename + L" function "
                    + position.getFunctionName() + L" (line " + std::to_wstring(position.getLine())
                    + L")\n";
            } else {
                message = std::wstring(L"In ") + filename + L" (line "
                    + std::to_wstring(position.getLine()) + L")\n";
            }
        }
        io->outputMessage(message);
    }
    io->outputMessage(L"\n");
}
//=============================================================================
ArrayOfVector
Nelson::DebuggerGateway::dbstackBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    int nbOmits = 1;
    bool withCompleteNames = false;
    stackTrace positions;
    switch (argIn.size()) {
    case 0: {
    } break;
    case 1: {
        bool isCompleteNames = false;
        bool isNbOmits = false;
        bool _withCompleteNames = false;
        int _nbOmits = 1;
        checkArgument(eval, argIn[0], _withCompleteNames, _nbOmits, isCompleteNames, isNbOmits);
        if (isCompleteNames) {
            withCompleteNames = true;
        }
        if (isNbOmits) {
            nbOmits = _nbOmits;
        }
    } break;
    case 2: {
        bool isCompleteNames = false;
        bool isNbOmits = false;
        bool _withCompleteNames = false;
        int _nbOmits = 1;
        bool _wasWithCompleteNames = false;
        bool _wasNbOmits = false;
        checkArgument(eval, argIn[0], _withCompleteNames, _nbOmits, isCompleteNames, isNbOmits);
        if (isCompleteNames) {
            withCompleteNames = true;
        }
        if (isNbOmits) {
            nbOmits = _nbOmits;
        }
        checkArgument(eval, argIn[1], _withCompleteNames, _nbOmits, isCompleteNames, isNbOmits);
        if (isCompleteNames) {
            withCompleteNames = true;
        }
        if (isNbOmits) {
            nbOmits = _nbOmits;
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }

    CallStack callstack;
    if (eval != nullptr) {
        callstack = eval->callstack;
    }
    DebugStack(callstack, nbOmits, positions);

    switch (nLhs) {
    case 0: {
        if (eval != nullptr) {
            Interface* io = eval->getInterface();
            if (io != nullptr) {
                dbstackPrint(io, positions, withCompleteNames);
            }
        }
    } break;
    case 1: {
        retval << dbstackAsStruct(positions, withCompleteNames);
    } break;
    case 2: {
        retval << dbstackAsStruct(positions, withCompleteNames);
        auto indexWorkspace = static_cast<double>(positions.size());
        if (indexWorkspace <= 0) {
            indexWorkspace = 1;
        }
        retval << ArrayOf::doubleConstructor(indexWorkspace);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
