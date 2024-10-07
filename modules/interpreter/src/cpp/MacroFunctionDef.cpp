//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <regex>
#include "MacroFunctionDef.hpp"
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
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "OverloadHelpers.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
MacroFunctionDef::MacroFunctionDef() : FunctionDef(false)
{
    this->localFunction = false;
    this->nextFunction = nullptr;
    this->prevFunction = nullptr;
    this->code = nullptr;
    this->ptrAstCodeAsVector.clear();
    this->isScript = false;
    this->withWatcher = false;
}
//=============================================================================
MacroFunctionDef::MacroFunctionDef(const std::wstring& filename, bool withWatcher, bool isOverload)
    : FunctionDef(isOverload)
{
    this->localFunction = false;
    this->nextFunction = nullptr;
    this->prevFunction = nullptr;
    this->code = nullptr;
    this->setFilename(filename);
    updateCode(); //-V1053
    this->withWatcher = withWatcher;
}
//=============================================================================
MacroFunctionDef::~MacroFunctionDef()
{
    if (nextFunction != nullptr) {
        delete nextFunction;
        nextFunction = nullptr;
    }
    AbstractSyntaxTree::deleteReferences(ptrAstCodeAsVector);
    code = nullptr;
    localFunction = false;
    isScript = false;
    withWatcher = false;
}
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
ArrayOfVector
MacroFunctionDef::evaluateMFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    if (eval->withOverload && inputs.size() > 0 && !this->isOverload()
        && this->overloadAutoMode == NLS_OVERLOAD_AUTO_ON) {
        bool wasFound = false;
        ArrayOfVector res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), nargout, inputs,
            getName(), ClassName(inputs[0]), inputs[0].getDataClass(), wasFound);
        if (wasFound) {
            return res;
        }
    }

    ArrayOfVector outputs;
    size_t minCount = 0;
    Context* context = eval->getContext();
    context->pushScope(this->getName(), this->getFilename());

    std::string filenameUtf8 = wstring_to_utf8(this->getFilename());
    eval->callstack.pushDebug(filenameUtf8, this->getName());
    // Push our local functions onto the function scope
    MacroFunctionDef* cp = this;
    while (cp->prevFunction != nullptr) {
        cp = cp->prevFunction;
    }
    cp = cp->nextFunction;
    while (cp != nullptr) {
        context->insertMacroFunctionLocally((FunctionDefPtr)cp);
        cp = cp->nextFunction;
    }
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
    context->getCurrentScope()->setNargOut(nargout);
    uint64 tic = 0;
    try {
        uint64 tic = Profiler::getInstance()->tic();
        eval->block(code);
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        State state(eval->getState());
        if (state < NLS_STATE_QUIT) {
            eval->resetState();
        }
        if (state == NLS_STATE_ABORT) {
            return scalarArrayOfToArrayOfVector(ArrayOf::emptyConstructor());
        }
        bool warningIssued = false;
        if (outputArgCount() != -1) {
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
            }
        } else {
            ArrayOf varargout;
            bool haveVarargout = context->lookupVariableLocally("varargout", varargout);
            if (haveVarargout) {
                if (varargout.getDataClass() != NLS_CELL_ARRAY) {
                    context->popScope();
                    eval->callstack.popDebug();
                    Error(_W("The special variable 'varargout' was not defined as a "
                             "cell-array."));
                }
            }
            indexType varlen = varargout.getElementCount();
            int explicitCount = static_cast<int>(returnVals.size()) - 1;
            bool noArgs = (explicitCount == 0 && varlen == 0);
            if (!noArgs && !haveVarargout) {
                context->popScope();
                eval->callstack.popDebug();
                Error(_W("The special variable 'varargout' was not defined as expected."));
            }
            if (explicitCount == 0 && varlen > 0 && nargout < 2) {
                indexType toFill = 1;
                outputs.resize(toFill);
                const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.getDataPointer()));
                // Get the length
                if (static_cast<indexType>(toFill)
                    > static_cast<indexType>(varargout.getElementCount())) {
                    Error(_W("Not enough outputs in varargout to satisfy call."));
                }
                outputs[0] = dp[0];
                outputs[0].name("");
            } else {
                outputs.resize(nargout);
                // For each explicit argument (that we have), insert it
                // into the scope.
                ArrayOf a;
                for (indexType i = 0; i < (indexType)explicitCount; i++) {
                    if (!context->lookupVariableLocally(returnVals[i], a)) {
                        if (!warningIssued) {
                            Warning(_W("one or more outputs not assigned in call."));
                            warningIssued = true;
                        }
                        a = ArrayOf::emptyConstructor();
                    }
                    outputs[i] = a;
                }
                // Are there any outputs not yet filled?
                if (nargout > explicitCount) {
                    // Get the data pointer
                    const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.getDataPointer()));
                    // Get the length
                    int toFill = nargout - explicitCount;
                    if (static_cast<double>(toFill) > static_cast<double>(varlen)) {
                        Error(_W("Not enough outputs in varargout to satisfy call."));
                    }
                    for (int i = 0; i < toFill; i++) {
                        outputs[explicitCount + i] = dp[i];
                    }
                }
            }
        }
        context->popScope();
        eval->callstack.popDebug();
    } catch (const Exception&) {
        internalProfileFunction stack
            = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
        Profiler::getInstance()->toc(tic, stack);
        context->popScope();
        eval->callstack.popDebug();
        throw;
    }
    for (size_t k = 0; k < outputs.size(); ++k) {
        outputs[k].name("");
    }
    return outputs;
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateMScript(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    ArrayOfVector outputs;
    Context* context = eval->getContext();
    std::string filenameUtf8 = wstring_to_utf8(this->getFilename());
    eval->callstack.pushDebug(filenameUtf8, this->getName());

    context->getCurrentScope()->setInputArgumentNames(stringVector());
    context->getCurrentScope()->setNargIn(0);
    context->getCurrentScope()->setNargOut(0);

    uint64 tic = 0;
    try {
        uint64 tic = Profiler::getInstance()->tic();
        eval->block(code);
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
        State state(eval->getState());
        if (state < NLS_STATE_QUIT) {
            eval->resetState();
        }
        if (state == NLS_STATE_ABORT) {
            return scalarArrayOfToArrayOfVector(ArrayOf::emptyConstructor());
        }
        eval->callstack.popDebug();
    } catch (const Exception&) {
        internalProfileFunction stack
            = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
        Profiler::getInstance()->toc(tic, stack);
        eval->callstack.popDebug();
        throw;
    }
    return outputs;
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    lock();
    updateCode();
    if (isScript) {
        return evaluateMScript(eval, inputs, nargout);
    }
    return evaluateMFunction(eval, inputs, nargout);
}
//=============================================================================
std::string
MacroFunctionDef::getCompleteName()
{
    if (this->localFunction) {
        MacroFunctionDef* pF = this->prevFunction;
        while (pF != nullptr) {
            if (!pF->localFunction) {
                return pF->getName() + ">" + this->getName();
            }
            pF = pF->prevFunction;
        }
    }
    return this->getName();
}
//=============================================================================
bool
MacroFunctionDef::updateCode()
{
    bool forceUpdate = this->code == nullptr;
    bool doUpdate = (forceUpdate || withWatcher) && (!this->localFunction);

    if (!doUpdate) {
        return false;
    }
    std::string errorMessage;
    if (errorMessage.empty()) {
        if (!forceUpdate) {
            return false;
        }
    } else {
        return false;
    }
    if (code != nullptr) {
        for (auto ptr : this->ptrAstCodeAsVector) {
            delete ptr;
        }
        ptrAstCodeAsVector.clear();
        code = nullptr;
    }
    FILE* fr;
#ifdef _MSC_VER
    fr = _wfopen(this->getFilename().c_str(), L"rt");
#else
    fr = fopen(wstring_to_utf8(this->getFilename()).c_str(), "r");
#endif
    if (fr == nullptr) {
        int errnum = errno;
        std::string msg1 = fmt::sprintf(_("Value of errno: %d"), errno);
        std::string msg2 = fmt::sprintf(_("Error opening file: %s"), strerror(errnum));
        Error(_W("Cannot open:") + L" " + this->getFilename() + L"\n" + utf8_to_wstring(msg1)
            + L"\n" + utf8_to_wstring(msg2));
    }

    ParserState pstate = ParserState::ParseError;
    AbstractSyntaxTree::clearReferences();
    AbstractSyntaxTreePtrVector ptAstCode;

    if (this->isOverload() && StringHelpers::ends_with(this->getFilename(), L"/end.m")) {

        std::string fileContent;
        char buffer[4096];

        while (fgets(buffer, sizeof(buffer), fr)) {
            fileContent += buffer;
        }
        fclose(fr);

        std::regex pattern(R"(\s*=\s*end\s*\()");
        fileContent = std::regex_replace(fileContent, pattern, " = endmagic(");

        try {
            pstate = parseString(fileContent);
            ptAstCode = AbstractSyntaxTree::getReferences();
        } catch (const Exception&) {
            AbstractSyntaxTree::deleteReferences();
            throw;
        }
    } else {
        try {
            pstate = parseFile(fr, wstring_to_utf8(this->getFilename()));
            ptAstCode = AbstractSyntaxTree::getReferences();
        } catch (const Exception&) {
            AbstractSyntaxTree::deleteReferences();
            if (fr != nullptr) {
                fclose(fr);
            }
            throw;
        }
        if (fr != nullptr) {
            fclose(fr);
        }
        if (pstate == ParserState::ParseError) {
            AbstractSyntaxTree::deleteReferences(ptAstCode);
            AbstractSyntaxTree::clearReferences();
            Error(_W("a valid function definition expected.") + std::wstring(L"\n")
                + this->getFilename());
        }
    }
    try {
        if (pstate == ParserState::FuncDef) {
            MacroFunctionDef* macroFunctionDef = getParsedFunctionDef();
            this->setIsScript(false);
            if (macroFunctionDef == nullptr) {
                FileSystemWrapper::Path pathFunction(this->getFilename());
                this->setName(pathFunction.stem().generic_string());
            } else {
                this->code = macroFunctionDef->code;
                this->arguments = macroFunctionDef->arguments;
                this->localFunction = macroFunctionDef->localFunction;
                this->nextFunction = macroFunctionDef->nextFunction;
                this->prevFunction = macroFunctionDef->prevFunction;
                this->returnVals = macroFunctionDef->returnVals;
                this->ptrAstCodeAsVector = macroFunctionDef->ptrAstCodeAsVector;
                this->setName(macroFunctionDef->getName());
            }
        } else {
            this->code = getParsedScriptBlock();
            this->arguments.clear();
            this->localFunction = false;
            this->nextFunction = nullptr;
            this->prevFunction = nullptr;
            this->returnVals.clear();
            this->setIsScript(true);
            FileSystemWrapper::Path pathFunction(this->getFilename());
            this->setName(pathFunction.stem().generic_string());
        }
    } catch (const Exception&) {
        Error(_W("a valid function definition expected.") + std::wstring(L"\n")
            + this->getFilename());
    }
    this->ptrAstCodeAsVector = std::move(ptAstCode);
    AbstractSyntaxTree::clearReferences();

    if (!this->getIsScript()) {
        stringVector functionNamesInFile;
        MacroFunctionDef* cp = this->nextFunction;
        functionNamesInFile.push_back(this->getName());
        while (cp != nullptr) {
            functionNamesInFile.push_back(cp->getName());
            cp = cp->nextFunction;
        }

        auto it = std::unique(functionNamesInFile.begin(), functionNamesInFile.end());
        bool isUnique = (it == functionNamesInFile.end());
        if (!isUnique) {
            std::string msg = fmt::sprintf(
                _("Function '%s' has already been declared within this scope."), it->c_str());
            Error(msg);
        }

        FileSystemWrapper::Path pathFunction(this->getFilename());
        const std::string functionNameFromFile = pathFunction.stem().generic_string();

        if (this->getName() != functionNameFromFile) {
            if (std::find(
                    functionNamesInFile.begin(), functionNamesInFile.end(), functionNameFromFile)
                != functionNamesInFile.end()) {
                this->setName(functionNameFromFile);
            }
        }
        if ((this->getName() != functionNameFromFile) && (functionNameFromFile != "end")) {
            std::string name = this->getName();
            std::string msg = fmt::sprintf(_("Filename and function name are not same (%s vs %s)."),
                name, functionNameFromFile);
            Error(msg);
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
