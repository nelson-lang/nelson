//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "AnonymousMacroFunctionDef.hpp"
#include "StringHelpers.hpp"
#include "Context.hpp"
#include "ParserInterface.hpp"
#include "Warning.hpp"
#include "Evaluator.hpp"
#include "characters_encoding.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
AnonymousMacroFunctionDef::AnonymousMacroFunctionDef(const std::string& functionHandle)
    : FunctionDef(false)
{
    this->code = nullptr;
    this->isFunctionHandleOnly = true;
    this->functionHandleContent = functionHandle;
    if (!functionHandle.empty()) {
        StringHelpers::trim_left(functionHandleContent);
        StringHelpers::trim_right(functionHandleContent);
        if (!functionHandleContent.empty() && functionHandleContent[0] == '@') {
            functionHandleContent.erase(functionHandleContent.begin());
        }
        setName(functionHandleContent);
    }
    arguments.clear();
}
//=============================================================================
AnonymousMacroFunctionDef::AnonymousMacroFunctionDef(const std::string& anonymousContent,
    const stringVector& arguments, const stringVector& variableNames,
    const std::vector<ArrayOf>& variables)
    : FunctionDef(false)
{
    this->code = nullptr;
    this->isFunctionHandleOnly = false;
    this->anonymousContent = anonymousContent;
    this->arguments = arguments;
    this->variableNames = variableNames;
    this->variables = variables;
}
//=============================================================================
AnonymousMacroFunctionDef::~AnonymousMacroFunctionDef()
{
    this->anonymousContent.clear();
    this->functionHandleContent.clear();
    this->previousLhs = -1;
    this->code = nullptr;
    this->isFunctionHandleOnly = false;
    this->arguments.clear();
    this->variableNames.clear();
    this->variables.clear();
}
//=============================================================================
std::string
AnonymousMacroFunctionDef::getDefinition()
{
    if (isFunctionHandleOnly) {
        return "@" + functionHandleContent;
    }
    std::string content = "@(";
    size_t nbArguments = arguments.size();
    for (size_t k = 0; k < arguments.size(); ++k) {
        content = content + arguments[k];
        if (k < nbArguments - 1) {
            content = content + ",";
        }
    }
    content = content + ") ";
    content = content + anonymousContent;
    return content;
}
//=============================================================================
stringVector
AnonymousMacroFunctionDef::getVariableNames()
{
    return variableNames;
}
//=============================================================================
std::vector<ArrayOf>
AnonymousMacroFunctionDef::getVariables()
{
    return variables;
}
//=============================================================================
int
AnonymousMacroFunctionDef::nargin()
{
    return inputArgCount();
}
//=============================================================================
int
AnonymousMacroFunctionDef::inputArgCount()
{
    if (isFunctionHandleOnly) {
        return -1;
    }
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
AnonymousMacroFunctionDef::nargout()
{
    return outputArgCount();
}
//=============================================================================
int
AnonymousMacroFunctionDef::outputArgCount()
{
    return -1;
}
//=============================================================================
ArrayOfVector
AnonymousMacroFunctionDef::evaluateFunction(
    Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    if (isFunctionHandleOnly) {
        FunctionDef* funcDef = nullptr;
        eval->lookupFunction(functionHandleContent, funcDef);
        if (funcDef) {
            return funcDef->evaluateFunction(eval, inputs, nargout);
        }
        std::string msg = functionHandleContent.empty()
            ? _("Undefined function.")
            : _("Undefined variable or function:") + functionHandleContent;
        std::string id = "Nelson:UndefinedFunction";
        Error(msg, id);
    }
    updateCode(nargout);
    ArrayOfVector outputs;
    size_t minCount = 0;
    Context* context = eval->getContext();
    context->pushScope(this->getName());

    std::string filenameUtf8 = "";
    eval->callstack.pushDebug(filenameUtf8, this->getName());
    stringVector inputNames;
    inputNames.reserve(inputs.size());
    for (const auto& input : inputs) {
        inputNames.push_back(input.name());
    }
    context->getCurrentScope()->setInputArgumentNames(inputNames);
    // When the function is called, the number of inputs is
    // at sometimes less than the number of arguments requested.
    // Check the argument count.  If this is a non-varargin
    // argument function, then use the following logic:
    minCount = 0;
    if (inputArgCount() != -1) {
        if (inputs.size() > arguments.size()) {
            context->popScope();
            eval->callstack.popDebug();
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        minCount = (inputs.size() < arguments.size()) ? inputs.size() : arguments.size();
        for (size_t i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            context->insertVariableLocally(arg, inputs[i]);
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(minCount));
    } else {
        // Count the number of supplied arguments
        size_t inputCount = inputs.size();
        size_t nbArgumentsWithoutVarArgIn = arguments.size();
        if (arguments[arguments.size() - 1] == "varargin") {
            nbArgumentsWithoutVarArgIn = nbArgumentsWithoutVarArgIn - 1;
        }
        if (inputCount < nbArgumentsWithoutVarArgIn) {
            context->popScope();
            eval->callstack.popDebug();
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(inputCount));
        // Get the number of explicit arguments
        int explicitCount = static_cast<int>(arguments.size()) - 1;
        // For each explicit argument (that we have an input for),
        // insert it into the scope.
        minCount = (explicitCount < static_cast<int>(inputCount))
            ? static_cast<size_t>(explicitCount)
            : inputCount;
        size_t i;
        for (i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            context->insertVariableLocally(arg, inputs[i]);
        }
        inputCount -= minCount;
        // Put minCount...inputCount
        ArrayOf varg(NLS_CELL_ARRAY);
        varg.vectorResize(inputCount);
        auto* dp = static_cast<ArrayOf*>(varg.getReadWriteDataPointer());
        for (i = 0; i < inputCount; i++) {
            dp[i] = inputs[i + minCount];
        }
        varg.name("varargin");
        context->insertVariableLocally("varargin", varg);
    }
    for (size_t k = 0; k < variableNames.size(); k++) {
        context->insertVariableLocally(variableNames[k], variables[k]);
    }

    context->getCurrentScope()->setNargOut(nargout);
    uint64 tic = 0;
    bool backupEcho = eval->getEchoMode();
    eval->setEchoMode(false);
    try {
        tic = Profiler::getInstance()->tic();
        eval->block(code);
        eval->setEchoMode(backupEcho);
        if (tic != 0) {
            internalProfileFunction stack = computeProfileStack(eval, getName(), L"", false);
            Profiler::getInstance()->toc(tic, stack);
        }
        State state(eval->getState());
        if (state < NLS_STATE_QUIT) {
            eval->resetState();
        }
        if (state == NLS_STATE_ABORT) {
            return scalarArrayOfToArrayOfVector(ArrayOf::emptyConstructor());
        }
        if (nargout == 0) {
            ArrayOf a;
            if (context->lookupVariableLocally("ans", a)) {
                outputs.resize(1);
                outputs[0] = a;
            }
        } else {
            bool warningIssued = false;
            outputs.resize(returnVals.size());
            ArrayOf a;
            for (size_t i = 0; i < returnVals.size(); i++) {
                if (!context->lookupVariableLocally(returnVals[i], a)) {
                    if (!warningIssued) {
                        std::wstring message = fmt::sprintf(
                            _W("Function : '%s'."), utf8_to_wstring(this->getName()));
                        message = message + L"\n" + WARNING_OUTPUTS_NOT_ASSIGNED;
                        Warning(message);
                        warningIssued = true;
                    }
                    a = ArrayOf::emptyConstructor();
                }
                outputs[i] = a;
                outputs[i].name("");
            }
        }

        context->popScope();
        eval->callstack.popDebug();
    } catch (const Exception&) {
        internalProfileFunction stack = computeProfileStack(eval, getName(), L"", false);
        Profiler::getInstance()->toc(tic, stack);
        eval->setEchoMode(backupEcho);
        context->popScope();
        eval->callstack.popDebug();
        throw;
    }
    return outputs;
}
//=============================================================================
std::string
AnonymousMacroFunctionDef::convertToStandardFunction(int nLhs)
{
    if (!isFunctionHandleOnly) {
        std::string functionReworked;
        std::string outputVariablesList;
        if (nLhs == 1) {
            outputVariablesList = "_VAR_ANONYMOUS_";
            outputVariablesList += " = ";

        } else {
            for (int k = 0; k < nLhs; ++k) {
                if (k == 0) {
                    outputVariablesList = "[_VAR_ANONYMOUS_" + std::to_string(k);
                } else {
                    outputVariablesList += ", _VAR_ANONYMOUS_" + std::to_string(k);
                }
            }
            if (nLhs > 0) {
                outputVariablesList += "] = ";
            }
        }
        functionReworked = "function " + outputVariablesList + "anonymousFunction(";
        size_t nbArguments = arguments.size();
        for (size_t k = 0; k < arguments.size(); ++k) {
            functionReworked = functionReworked + arguments[k];
            if (k < nbArguments - 1) {
                functionReworked = functionReworked + ",";
            }
        }
        functionReworked = functionReworked + ")" + "\n";
        functionReworked = functionReworked + outputVariablesList + anonymousContent + ";" + "\n";
        return functionReworked;
    }
    return "";
}
//=============================================================================
bool
AnonymousMacroFunctionDef::updateCode(int nLhs)
{
    if (isFunctionHandleOnly) {
        return true;
    }
    bool needToUpdate = (code == nullptr) || (this->previousLhs != nLhs);
    if (!needToUpdate) {
        return false;
    }
    this->previousLhs = nLhs;
    if (code != nullptr) {
        for (auto ptr : this->ptrAstCodeAsVector) {
            delete ptr;
        }
        ptrAstCodeAsVector.clear();
        code = nullptr;
    }
    ParserState pstate = ParserState::ParseError;
    AbstractSyntaxTree::clearReferences();
    AbstractSyntaxTreePtrVector ptAstCode;
    try {
        pstate = parseString(convertToStandardFunction(nLhs));
        ptAstCode = AbstractSyntaxTree::getReferences();
    } catch (const Exception&) {
        AbstractSyntaxTree::deleteReferences();
        throw;
    }
    if (pstate == ParserState::ParseError) {
        AbstractSyntaxTree::deleteReferences(ptAstCode);
        AbstractSyntaxTree::clearReferences();
        Error(_W("a valid function definition expected."));
    }

    MacroFunctionDef* macroFunctionDef = getParsedFunctionDef();
    if (macroFunctionDef) {
        if (macroFunctionDef->code && macroFunctionDef->code->down
            && macroFunctionDef->code->down->right != nullptr) {
            macroFunctionDef->code = nullptr;
            delete macroFunctionDef;
            macroFunctionDef = nullptr;
            Error("simple expression expected.");
            return false;
        }
        this->code = macroFunctionDef->code;
        this->arguments = macroFunctionDef->arguments;
        this->setName("Anonymous");
        this->returnVals = macroFunctionDef->returnVals;
        macroFunctionDef->code = nullptr;
        delete macroFunctionDef;
        return true;
    }

    return false;
}
//=============================================================================
bool
AnonymousMacroFunctionDef::updateCode()
{
    if (isFunctionHandleOnly) {
        return true;
    }
    return updateCode(1);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
