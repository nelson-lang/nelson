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
#include "LexerContext.hpp"
#include "OnCleanupObjectHandle.hpp"
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
    updateCode();
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
void
MacroFunctionDef::onCleanup(Evaluator* eval)
{
    for (auto task : cleanupTasks) {
        if (task.isHandle() && task.getHandleClassName() == NLS_HANDLE_ONCLEANUP_CATEGORY_STR) {
            OnCleanupObjectHandle* obj = (OnCleanupObjectHandle*)task.getContentAsHandleScalar();
            if (obj && obj->isScoped()) {
                obj->cleanup(eval);
            }
        }
    }
    cleanupTasks.clear();
}
//=============================================================================
ArrayOfVector
MacroFunctionDef::evaluateMFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    struct RecursionGuard
    {
        int& depth;
        explicit RecursionGuard(int& d) : depth(d) { ++depth; }
        ~RecursionGuard() { --depth; }
        RecursionGuard(const RecursionGuard&) = delete;
        RecursionGuard&
        operator=(const RecursionGuard&)
            = delete;
    };
    static thread_local int recursionDepth = 0;
    RecursionGuard rg(recursionDepth);

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
    Context* context = eval->getContext();
    context->pushScope(this->getName(), this->getFilename());

    std::string filenameUtf8 = wstring_to_utf8(this->getFilename());
    eval->callstack.pushDebug(filenameUtf8, this->getName());

    uint64 tic = 0;
    try {
        insertLocalFunctions(context);
        setInputArgumentNames(context, inputs);
        bindInputs(context, inputs);
        context->getCurrentScope()->setNargOut(nargout);

        // Only profile the outermost frame of a recursive chain to reduce overhead.
        if (recursionDepth == 1) {
            tic = Profiler::getInstance()->tic();
        }
        eval->block(code);
        if (recursionDepth == 1 && tic != 0) {
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

        outputs = prepareOutputs(context, nargout);

        onCleanup(eval);

        context->popScope();
        eval->callstack.popDebug();
    } catch (const Exception&) {
        onCleanup(eval);
        if (recursionDepth == 1 && tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
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

    // Apply the same recursion-depth guard for profiling in scripts.
    struct RecursionGuard
    {
        int& depth;
        explicit RecursionGuard(int& d) : depth(d) { ++depth; }
        ~RecursionGuard() { --depth; }
        RecursionGuard(const RecursionGuard&) = delete;
        RecursionGuard&
        operator=(const RecursionGuard&)
            = delete;
    };
    static thread_local int recursionDepthScript = 0;
    RecursionGuard rgs(recursionDepthScript);

    context->getCurrentScope()->setInputArgumentNames(stringVector());
    context->getCurrentScope()->setNargIn(0);
    context->getCurrentScope()->setNargOut(0);

    uint64 tic = 0;
    try {
        if (recursionDepthScript == 1) {
            tic = Profiler::getInstance()->tic();
        }
        eval->block(code);
        if (recursionDepthScript == 1 && tic != 0) {
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
        if (recursionDepthScript == 1 && tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, getCompleteName(), this->getFilename(), false);
            Profiler::getInstance()->toc(tic, stack);
        }
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

    resetCodeStorage();

    FILE* fr = openSourceFile();

    AbstractSyntaxTree::clearReferences();
    AbstractSyntaxTreePtrVector ptAstCode;
    Evaluator* eval = (Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator();
    ParserState pstate = parseSource(eval, fr, ptAstCode);

    if (pstate == ParserState::ParseError) {
        AbstractSyntaxTree::deleteReferences(ptAstCode);
        AbstractSyntaxTree::clearReferences();
        Error(_W("a valid function definition expected.") + std::wstring(L"\n")
            + this->getFilename());
    }

    try {
        assignParsedResult(pstate);
    } catch (const Exception&) {
        Error(_W("a valid function definition expected.") + std::wstring(L"\n")
            + this->getFilename());
    }

    this->ptrAstCodeAsVector = std::move(ptAstCode);
    AbstractSyntaxTree::clearReferences();

    if (!this->getIsScript()) {
        validateFunctionNamesAndFilename();
    }
    return true;
}
//=============================================================================
void
MacroFunctionDef::insertLocalFunctions(Context* context)
{
    MacroFunctionDef* cp = this;
    while (cp->prevFunction != nullptr) {
        cp = cp->prevFunction;
    }
    cp = cp->nextFunction;
    while (cp != nullptr) {
        context->insertMacroFunctionLocally((FunctionDefPtr)cp);
        cp = cp->nextFunction;
    }
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
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        size_t minCount = (inputs.size() < arguments.size()) ? inputs.size() : arguments.size();
        for (size_t i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            context->insertVariableLocally(arg, inputs[i]);
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(minCount));
    } else {
        size_t inputCount = inputs.size();
        size_t nbArgumentsWithoutVarArgIn = arguments.size();
        if (arguments[arguments.size() - 1] == "varargin") {
            nbArgumentsWithoutVarArgIn = nbArgumentsWithoutVarArgIn - 1;
        }
        if (inputCount < nbArgumentsWithoutVarArgIn) {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        }
        context->getCurrentScope()->setNargIn(static_cast<int>(inputCount));
        int explicitCount = static_cast<int>(arguments.size()) - 1;
        size_t minCount = (explicitCount < static_cast<int>(inputCount))
            ? static_cast<size_t>(explicitCount)
            : inputCount;
        for (size_t i = 0; i < minCount; i++) {
            std::string arg(arguments[i]);
            context->insertVariableLocally(arg, inputs[i]);
        }
        inputCount -= minCount;
        ArrayOf varg(NLS_CELL_ARRAY);
        varg.vectorResize(inputCount);
        auto* dp = static_cast<ArrayOf*>(varg.getReadWriteDataPointer());
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
        bool warningIssued = false;
        for (size_t i = 0; i < returnVals.size(); i++) {
            if (!context->lookupVariableLocally(returnVals[i], a)) {
                if (!warningIssued) {
                    std::wstring message
                        = fmt::sprintf(_W("Function : '%s'."), utf8_to_wstring(this->getName()));
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
                Error(_W("The special variable 'varargout' was not defined as a cell-array."));
            }
        }
        indexType varlen = varargout.getElementCount();
        int explicitCount = static_cast<int>(returnVals.size()) - 1;
        bool noArgs = (explicitCount == 0 && varlen == 0);
        if (!noArgs && !haveVarargout) {
            Error(_W("The special variable 'varargout' was not defined as expected."));
        }
        if (explicitCount == 0 && varlen > 0 && nargout < 2) {
            indexType toFill = 1;
            outputs.resize(toFill);
            const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.getDataPointer()));
            if (static_cast<indexType>(toFill)
                > static_cast<indexType>(varargout.getElementCount())) {
                Error(_W("Not enough outputs in varargout to satisfy call."));
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
                        Warning(_W("one or more outputs not assigned in call."));
                        warningIssued = true;
                    }
                    a = ArrayOf::emptyConstructor();
                }
                outputs[i] = a;
            }
            if (nargout > explicitCount) {
                const ArrayOf* dp = (static_cast<const ArrayOf*>(varargout.getDataPointer()));
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
    return outputs;
}
//=============================================================================
void
MacroFunctionDef::resetCodeStorage()
{
    if (code != nullptr) {
        for (auto ptr : this->ptrAstCodeAsVector) {
            delete ptr;
        }
        ptrAstCodeAsVector.clear();
        code = nullptr;
    }
}
//=============================================================================
FILE*
MacroFunctionDef::openSourceFile()
{
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
    return fr;
}
//=============================================================================
ParserState
MacroFunctionDef::parseSource(Evaluator* eval, FILE* fr, AbstractSyntaxTreePtrVector& ptAstCode)
{
    if (this->isOverload() && StringHelpers::ends_with(this->getFilename(), L"/end.m")) {
        return parseOverloadEndFunction(eval, fr, ptAstCode);
    } else {
        return parseRegularFile(eval, fr, ptAstCode);
    }
}
//=============================================================================
ParserState
MacroFunctionDef::parseOverloadEndFunction(
    Evaluator* eval, FILE* fr, AbstractSyntaxTreePtrVector& ptAstCode)
{
    std::string fileContent;
    char buffer[4096];
    while (fgets(buffer, sizeof(buffer), fr)) {
        fileContent += buffer;
    }
    fclose(fr);

    std::regex pattern(R"(\s*=\s*end\s*\()");
    fileContent = std::regex_replace(fileContent, pattern, " = endmagic(");

    LexerContext localContext;
    LexerContext& lexerCtx = getOrCreateLexerContext(eval, localContext);

    ParserState pstate = ParserState::ParseError;
    try {
        pstate = parseString(lexerCtx, fileContent);
        ptAstCode = AbstractSyntaxTree::getReferences();
    } catch (const Exception&) {
        AbstractSyntaxTree::deleteReferences();
        throw;
    }
    return pstate;
}
//=============================================================================
ParserState
MacroFunctionDef::parseRegularFile(
    Evaluator* eval, FILE* fr, AbstractSyntaxTreePtrVector& ptAstCode)
{
    LexerContext localContext;
    LexerContext& lexerCtx = getOrCreateLexerContext(eval, localContext);

    ParserState pstate = ParserState::ParseError;
    try {
        pstate = parseFile(lexerCtx, fr, wstring_to_utf8(this->getFilename()));
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
    return pstate;
}
//=============================================================================
LexerContext&
MacroFunctionDef::getOrCreateLexerContext(Evaluator* eval, LexerContext& localContext)
{
    if (eval) {
        return eval->lexerContext;
    }
    return localContext;
}
//=============================================================================
void
MacroFunctionDef::assignParsedResult(ParserState pstate)
{
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
}
//=============================================================================
void
MacroFunctionDef::validateFunctionNamesAndFilename()
{
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
        if (std::find(functionNamesInFile.begin(), functionNamesInFile.end(), functionNameFromFile)
            != functionNamesInFile.end()) {
            this->setName(functionNameFromFile);
        }
    }
    if ((this->getName() != functionNameFromFile) && (functionNameFromFile != "end")) {
        std::string name = this->getName();
        std::string msg = fmt::sprintf(
            _("Filename and function name are not same (%s vs %s)."), name, functionNameFromFile);
        Error(msg);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
