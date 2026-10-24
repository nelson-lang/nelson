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
#include "BytecodeArtifact.hpp"
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
MacroFunctionDef::MacroFunctionDef() : FunctionDef(false)
{
    this->localFunction = false;
    this->nestedFunction = false;
    this->nextFunction = nullptr;
    this->prevFunction = nullptr;
    this->code = nullptr;
    this->ptrAstCodeAsVector.clear();
    this->isScript = false;
    this->directScript = false;
    this->withWatcher = false;
}
//=============================================================================
MacroFunctionDef::MacroFunctionDef(
    const std::wstring& filename, bool withWatcher, bool isOverload, bool directScript)
    : FunctionDef(isOverload)
{
    this->localFunction = false;
    this->nestedFunction = false;
    this->nextFunction = nullptr;
    this->prevFunction = nullptr;
    this->code = nullptr;
    this->directScript = directScript;
    this->setFilename(filename);
    updateCode();
    this->withWatcher = withWatcher;
}
//=============================================================================
MacroFunctionDef::~MacroFunctionDef()
{
    bytecodeChunk.reset();
    if (nextFunction != nullptr) {
        delete nextFunction;
        nextFunction = nullptr;
    }
    AbstractSyntaxTree::deleteReferences(ptrAstCodeAsVector);
    code = nullptr;
    localFunction = false;
    nestedFunction = false;
    isScript = false;
    directScript = false;
    withWatcher = false;
}
//=============================================================================
bool
MacroFunctionDef::updateCode()
{
    bool forceUpdate = this->code == nullptr && this->bytecodeChunk == nullptr;
    bool doUpdate = (forceUpdate || withWatcher) && (!this->localFunction);
    if (!doUpdate) {
        return false;
    }
    if (forceUpdate && isBytecodeArtifact(getFilename())) {
        try {
            loadBytecodeArtifact(*this, getFilename());
        } catch (const std::exception& error) {
            RaiseError(error.what(), "Nelson:bytecode:invalidArtifact");
        }
        return true;
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
        Error(L"Nelson:interpreter:invalidFunctionDefinition",
            L"a valid function definition expected.\n%s", this->getFilename());
    }

    try {
        assignParsedResult(pstate);
    } catch (const NelsonException&) {
        Error(L"Nelson:interpreter:invalidFunctionDefinition",
            L"a valid function definition expected.\n%s", this->getFilename());
    }

    this->ptrAstCodeAsVector = std::move(ptAstCode);
    AbstractSyntaxTree::clearReferences();

    if (!this->getIsScript()) {
        validateFunctionNamesAndFilename();
    }
    invalidateBytecode();
    return true;
}
//=============================================================================
void
MacroFunctionDef::resetCodeStorage()
{
    invalidateBytecode();
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
        std::string msg1 = formatI18nMessage(
            resolveMessageTemplate("Nelson:interpreter:valueErrnoD", "Value of errno: %d"), errno);
        std::string msg2
            = formatI18nMessage(resolveMessageTemplate("Nelson:interpreter:errorOpeningFileS",
                                    "Error opening file: %s"),
                strerror(errnum));
        Error(L"Nelson:interpreter:cannotOpen", L"Cannot open: %s\n%s\n%s", this->getFilename(),
            utf8_to_wstring(msg1), utf8_to_wstring(msg2));
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
    } catch (const NelsonException&) {
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
    } catch (const NelsonException&) {
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
    // An empty file has no parsed definition; classify it as a script,
    // consistently with ParseFile, so it can also produce no-op bytecode.
    if (pstate == ParserState::FuncDef && getParsedFunctionDef() == nullptr) {
        pstate = ParserState::ScriptBlock;
    }
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
            this->nestedFunction = macroFunctionDef->nestedFunction;
            this->nextFunction = macroFunctionDef->nextFunction;
            this->prevFunction = macroFunctionDef->prevFunction;
            // The local functions parsed with the file are adopted above by
            // pointer, but each still points back at the parser's definition,
            // which this one supersedes and nothing owns. The owner of a file
            // is found by walking prevFunction up to the head (a function handle
            // created while the file runs pins that owner), so a chain whose
            // head is not the definition in use finds nobody.
            //
            // Only the links naming the parser's definition are moved. The same
            // field is the lexical parent of a nested function (appendFunctionChain
            // sets it on the first node of a nested block); re-pointing those
            // would flatten the nesting that computeCompleteName reads back as
            // parent>child.
            for (MacroFunctionDef* local = this->nextFunction; local != nullptr;
                local = local->nextFunction) {
                if (local->prevFunction == macroFunctionDef) {
                    local->prevFunction = this;
                }
            }
            this->returnVals = macroFunctionDef->returnVals;
            this->ptrAstCodeAsVector = macroFunctionDef->ptrAstCodeAsVector;
            this->setName(macroFunctionDef->getName());
        }
    } else {
        this->code = getParsedScriptBlock();
        this->arguments.clear();
        this->localFunction = false;
        this->nestedFunction = false;
        this->nextFunction = getParsedFunctionDef();
        this->prevFunction = nullptr;
        MacroFunctionDef* previousFunction = this;
        MacroFunctionDef* local = this->nextFunction;
        while (local != nullptr) {
            local->localFunction = true;
            local->prevFunction = previousFunction;
            local->setFilename(this->getFilename());
            previousFunction = local;
            local = local->nextFunction;
        }
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
        std::string msg = formatI18nMessage(
            Nelson::resolveMessageTemplate("Nelson:interpreter:functionHasAlreadyBeen",
                "Function '%s' has already been declared within this scope."),
            *it);
        Error("Nelson:interpreter:functionAlreadyDeclared", msg);
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
        // The filename is the callable name: a primary function declared under
        // a different name adopts the filename instead of rejecting the file.
        this->setName(functionNameFromFile);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
