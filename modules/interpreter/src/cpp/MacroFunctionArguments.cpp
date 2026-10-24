//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4996)
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <atomic>
#include <mutex>
#include <regex>
#include <unordered_map>
#include <unordered_set>
#include "BytecodeCompiler.hpp"
#include "BytecodeVM.hpp"
#include "ClassdefHandleFinalization.hpp"
#include "MacroFunctionDef.hpp"
#include "ClassdefParser.hpp"
#include "Context.hpp"
#include "FileParser.hpp"
#include "ParserInterface.hpp"
#include "Warning.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FunctionsInMemory.hpp"
#include "OverloadHelpers.hpp"
#include "StringHelpers.hpp"
#include "LexerContext.hpp"
#include "OnCleanupObjectHandle.hpp"
#include "ArgumentsBlockLowering.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
int
MacroFunctionDef::nargin()
{
    if (arguments.empty()) {
        return 0;
    }
    if (arguments[arguments.size() - 1] == "varargin") {
        return -static_cast<int>(arguments.size());
    }
    return static_cast<int>(arguments.size());
}
//=============================================================================
int
MacroFunctionDef::inputArgCount()
{
    if (arguments.empty()) {
        return 0;
    }
    if (arguments[arguments.size() - 1] == "varargin") {
        return -1;
    }
    return static_cast<int>(arguments.size());
}
//=============================================================================
int
MacroFunctionDef::nargout()
{
    if (returnVals.empty()) {
        return 0;
    }
    if (returnVals[returnVals.size() - 1] == "varargout") {
        return -static_cast<int>(returnVals.size());
    }
    return static_cast<int>(returnVals.size());
}
//=============================================================================
int
MacroFunctionDef::outputArgCount()
{
    if (returnVals.empty()) {
        return 0;
    }
    if (returnVals[returnVals.size() - 1] == "varargout") {
        return -1;
    }
    return static_cast<int>(returnVals.size());
}
//=============================================================================
void
MacroFunctionDef::setInputArgumentNames(Context* context, const ArrayOfVector& inputs)
{
    stringVector inputNames;
    inputNames.reserve(inputs.size());
    for (const auto& input : inputs) {
        inputNames.push_back(input.name());
    }
    context->getCurrentScope()->setInputArgumentNames(inputNames);
}
//=============================================================================
void
MacroFunctionDef::bindInputs(Context* context, const ArrayOfVector& inputs)
{
    if (inputArgCount() != -1) {
        if (inputs.size() > arguments.size()) {
            Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
        }
        size_t minCount = (inputs.size() < arguments.size()) ? inputs.size() : arguments.size();
        for (size_t i = 0; i < minCount; i++) {
            if (arguments[i] != "~") {
                context->insertVariableLocally(arguments[i], inputs[i]);
            }
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(minCount));
    } else {
        size_t inputCount = inputs.size();
        context->getCurrentScope()->setNargIn(static_cast<int>(inputCount));
        int explicitCount = static_cast<int>(arguments.size()) - 1;
        size_t minCount = (explicitCount < static_cast<int>(inputCount))
            ? static_cast<size_t>(explicitCount)
            : inputCount;
        for (size_t i = 0; i < minCount; i++) {
            if (arguments[i] != "~") {
                context->insertVariableLocally(arguments[i], inputs[i]);
            }
        }
        inputCount -= minCount;
        ArrayOf varg(NLS_CELL_ARRAY);
        varg.vectorResize(inputCount);
        auto* dp = static_cast<ArrayOf*>(varg.readWriteDataPointer());
        for (size_t i = 0; i < inputCount; i++) {
            dp[i] = inputs[i + minCount];
        }
        varg.name("varargin");
        context->insertVariableLocally("varargin", varg);
    }
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::prepareOutputs(Context* context, int nargout)
{
    ArrayOfVector outputs;
    if (outputArgCount() != -1) {
        outputs.resize(returnVals.size());
        ArrayOf a;
        for (size_t i = 0; i < returnVals.size(); i++) {
            if (!context->lookupVariableLocally(returnVals[i], a)) {
                // A requested output (index < nargout) that the body never
                // assigned is an error; outputs beyond nargout
                // are not required and stay empty.
                if (static_cast<int>(i) < nargout) {
                    std::string msg = formatI18nMessage(
                        Nelson::resolveMessageTemplate("Nelson:interpreter:outputNotAssigned",
                            "Output argument '%s' is not assigned during call to '%s'."),
                        returnVals[i], this->getName());
                    RaiseError(msg, "Nelson:interpreter:outputNotAssigned");
                }
                a = ArrayOf::empty();
            }
            outputs[i] = a;
        }
    } else {
        ArrayOf varargout;
        bool haveVarargout = context->lookupVariableLocally("varargout", varargout);
        if (haveVarargout) {
            if (varargout.dataClass() != NLS_CELL_ARRAY) {
                Error(L"Nelson:interpreter:invalidVarargout",
                    L"The special variable 'varargout' was not defined as a cell-array.");
            }
        }
        indexType varlen = varargout.elementCount();
        int explicitCount = static_cast<int>(returnVals.size()) - 1;
        // A defined varargout is required only when nargout exceeds the number
        // of named outputs. Otherwise an unset varargout is treated as empty and
        // raises no error.
        if (nargout > explicitCount && !haveVarargout) {
            Error(L"Nelson:interpreter:invalidVarargout",
                L"The special variable 'varargout' was not defined as a cell-array.");
        }
        if (explicitCount == 0 && varlen > 0 && nargout == 1) {
            indexType toFill = 1;
            outputs.resize(toFill);
            const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.dataPointer()));
            if (static_cast<indexType>(toFill) > static_cast<indexType>(varargout.elementCount())) {
                Error(L"Nelson:interpreter:notEnoughVarargout",
                    L"Not enough outputs in varargout to satisfy call.");
            }
            outputs[0] = dp[0];
            outputs[0].name("");
        } else {
            outputs.resize(nargout);
            ArrayOf a;
            bool warningIssued = false;
            for (indexType i = 0; i < (indexType)explicitCount; i++) {
                if (!context->lookupVariableLocally(returnVals[i], a)) {
                    if (!warningIssued) {
                        Warning(L"Nelson:outputsNotAssigned",
                            L"one or more outputs not assigned in call.");
                        warningIssued = true;
                    }
                    a = ArrayOf::empty();
                }
                outputs[i] = a;
            }
            if (nargout > explicitCount) {
                const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.dataPointer()));
                int toFill = nargout - explicitCount;
                if (static_cast<double>(toFill) > static_cast<double>(varlen)) {
                    Error(L"Nelson:interpreter:notEnoughVarargout",
                        L"Not enough outputs in varargout to satisfy call.");
                }
                for (int i = 0; i < toFill; i++) {
                    outputs[explicitCount + i] = dp[i];
                }
            }
        }
    }
    return outputs;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
