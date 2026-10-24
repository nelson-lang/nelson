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
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <algorithm>
#include <cstdio>
#include <functional>
#include <set>
#include <vector>
#include "Evaluator.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserDefinitionQuery.hpp"
#include "ClassdefParserHelpers.hpp"
#include "ClassdefParserRegistry.hpp"
#include "ErrorID.hpp"
#include "FunctionsInMemory.hpp"
#include "MacroFunctionDef.hpp"
#include "OverloadName.hpp"
#include "ParseFile.hpp"
#include "ParserInterface.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static size_t
maxLineFromChainHelper(
    AbstractSyntaxTreePtr t, std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc);
static size_t
getLineNumberFromCode(AbstractSyntaxTreePtr code, size_t lineNumber, size_t maxLinesInFile,
    std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc);
//=============================================================================
static std::set<size_t>
getMultilineCommentLines(const std::wstring& source)
{
    std::set<size_t> commentLines;
    bool inComment = false;
    size_t lineNum = 1;
    size_t pos = 0;
    while (pos < source.size()) {
        if (!inComment && source.substr(pos, 2) == L"%{") {
            inComment = true;
            pos += 2;
        }
        if (inComment && source.substr(pos, 2) == L"%}") {
            inComment = false;
            pos += 2;
        }
        if (inComment) {
            commentLines.insert(lineNum);
        }
        if (source[pos] == L'\n') {
            lineNum++;
        }
        pos++;
    }
    return commentLines;
}
//=============================================================================
static std::set<size_t>
getCommentOnlyLines(const std::wstring& sourceFilePath)
{
    std::set<size_t> commentLines;
    FILE* fp = nullptr;
#ifdef _MSC_VER
    fp = _wfopen(sourceFilePath.c_str(), L"rb");
#else
    fp = fopen(wstring_to_utf8(sourceFilePath).c_str(), "rb");
#endif
    if (!fp) {
        return commentLines;
    }

    bool inBlockComment = false;
    size_t lineNum = 1;
    char buffer[4096];
    while (fgets(buffer, sizeof(buffer), fp) != nullptr) {
        std::string line(buffer);
        while (!line.empty() && (line.back() == '\n' || line.back() == '\r')) {
            line.pop_back();
        }
        size_t first = 0;
        while (first < line.size() && (line[first] == ' ' || line[first] == '\t')) {
            first++;
        }
        std::string trimmed = line.substr(first);
        if (inBlockComment || (!trimmed.empty() && trimmed[0] == '%')) {
            commentLines.insert(lineNum);
        }
        if (trimmed.size() >= 2 && trimmed[0] == '%' && trimmed[1] == '{') {
            inBlockComment = true;
        }
        if (trimmed.size() >= 2 && trimmed[0] == '%' && trimmed[1] == '}') {
            inBlockComment = false;
        }
        lineNum++;
    }
    fclose(fp);
    return commentLines;
}
//=============================================================================
// Returns line numbers that are inside arguments...end blocks in the source.
// These lines contain generated validation code in the rewritten buffer and
// should not be direct breakpoint targets; breakpoints are forwarded past the block.
static std::set<size_t>
getArgumentsBlockLines(const std::wstring& sourceFilePath)
{
    std::set<size_t> blockLines;
    FILE* fp = nullptr;
#ifdef _MSC_VER
    fp = _wfopen(sourceFilePath.c_str(), L"rb");
#else
    fp = fopen(wstring_to_utf8(sourceFilePath).c_str(), "rb");
#endif
    if (!fp) {
        return blockLines;
    }
    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    if (fileSize <= 0) {
        fclose(fp);
        return blockLines;
    }
    std::string content(static_cast<size_t>(fileSize), '\0');
    fread(&content[0], 1, static_cast<size_t>(fileSize), fp);
    fclose(fp);

    bool inArgBlock = false;
    size_t lineNum = 1;
    size_t pos = 0;
    while (pos <= content.size()) {
        size_t eol = content.find('\n', pos);
        std::string rawLine
            = (eol == std::string::npos) ? content.substr(pos) : content.substr(pos, eol - pos);
        if (!rawLine.empty() && rawLine.back() == '\r') {
            rawLine.pop_back();
        }

        size_t i = 0;
        while (i < rawLine.size() && (rawLine[i] == ' ' || rawLine[i] == '\t')) {
            i++;
        }
        std::string trimmed = rawLine.substr(i);
        {
            char delim = '\0';
            size_t j = 0;
            while (j < trimmed.size()) {
                char ch = trimmed[j];
                if (delim != '\0') {
                    if (ch == delim) {
                        if (j + 1 < trimmed.size() && trimmed[j + 1] == delim) {
                            j += 2;
                            continue;
                        }
                        delim = '\0';
                    }
                } else if (ch == '\'' || ch == '"') {
                    delim = ch;
                } else if (ch == '%') {
                    trimmed = trimmed.substr(0, j);
                    break;
                }
                j++;
            }
            while (!trimmed.empty() && (trimmed.back() == ' ' || trimmed.back() == '\t')) {
                trimmed.pop_back();
            }
        }

        if (!inArgBlock) {
            // Input and output arguments blocks are both non-breakpointable.
            if (trimmed == "arguments"
                || (trimmed.size() > 9 && trimmed.substr(0, 9) == "arguments"
                    && (trimmed[9] == ' ' || trimmed[9] == '('))) {
                inArgBlock = true;
                blockLines.insert(lineNum);
            }
        } else {
            if (trimmed == "end") {
                blockLines.insert(lineNum);
                inArgBlock = false;
            } else {
                blockLines.insert(lineNum);
            }
        }

        if (eol == std::string::npos) {
            break;
        }
        pos = eol + 1;
        lineNum++;
    }
    return blockLines;
}
//=============================================================================
namespace {
    //=============================================================================
    void
    splitBreakpointName(const std::wstring& filename, std::wstring& mainFunctionName,
        std::wstring& subFunctionName, std::wstring& functionName)
    {
        mainFunctionName = filename;
        size_t separatorPos = filename.find(L'>');
        if (separatorPos != std::wstring::npos) {
            mainFunctionName = filename.substr(0, separatorPos);
            subFunctionName = filename.substr(separatorPos + 1);
        }
        std::wstring basename = mainFunctionName;
        size_t lastSlash = mainFunctionName.find_last_of(L"/\\");
        if (lastSlash != std::wstring::npos) {
            basename = mainFunctionName.substr(lastSlash + 1);
        }
        functionName = basename;
        if (functionName.length() > 2 && functionName.substr(functionName.length() - 2) == L".m") {
            functionName = functionName.substr(0, functionName.length() - 2);
        }
    }
    //=============================================================================
    MacroFunctionDef*
    findSubFunction(MacroFunctionDef* functionDef, const std::wstring& subFunctionName)
    {
        std::string asSubFunctionName = wstring_to_utf8(subFunctionName);
        MacroFunctionDef* searchFunc = functionDef->nextFunction;
        while (searchFunc != nullptr) {
            if (searchFunc->getName() == asSubFunctionName) {
                return searchFunc;
            }
            searchFunc = searchFunc->nextFunction;
        }
        return nullptr;
    }
    //=============================================================================
    MacroFunctionDef*
    selectFunctionForLine(MacroFunctionDef* functionDef, size_t requestedLine,
        size_t& maxLinesInFile, std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
    {
        MacroFunctionDef* targetFunc = nullptr;
        MacroFunctionDef* currentFunc = functionDef;
        while (currentFunc != nullptr) {
            size_t funcMax = maxLineFromChainHelper(currentFunc->code, getLineFunc);
            maxLinesInFile = std::max(maxLinesInFile, funcMax);
            size_t funcMin = currentFunc->code != nullptr ? getLineFunc(currentFunc->code) : 0;
            if ((funcMin > 0 && funcMin <= requestedLine && requestedLine <= funcMax)
                || (targetFunc == nullptr && funcMax > 0)) {
                targetFunc = currentFunc;
            }
            currentFunc = currentFunc->nextFunction;
        }
        return targetFunc;
    }
    //=============================================================================
    size_t
    countSourceLines(const std::string& source)
    {
        size_t lines = source.empty() ? 0 : 1;
        for (size_t k = 0; k + 1 < source.size(); ++k) {
            if (source[k] == '\n') {
                ++lines;
            }
        }
        return lines;
    }
    //=============================================================================
    // The name a class method answers to in the function tables.
    std::string
    classdefMethodFunctionName(
        const ClassdefDefinition& definition, const ClassdefMethodDefinition& method)
    {
        if (method.isStatic) {
            return definition.name + "." + method.name;
        }
        if (classdef_detail::equalsIgnoreCase(method.name, CLASSDEF_FUNCTION_DELETE)
            && classdef_detail::definitionIsHandleClass(definition)) {
            // a handle class exposes delete as the generated destructor
            // protocol; the written body keeps a mangled name of its own
            return CLASSDEF_GENERATED_FUNCTION_PREFIX + definition.name + "_"
                + CLASSDEF_FUNCTION_DELETE + CLASSDEF_GENERATED_FUNCTION_SUFFIX;
        }
        return getOverloadFunctionName(definition.name, method.name);
    }
    //=============================================================================
    // A class file compiles to one function per method, each carrying the line
    // numbers of the class file. None is reachable from the constructor: the
    // class name resolves to the constructor alone and its nextFunction chain
    // holds the local functions only. A requested line is therefore routed by
    // the source spans of the parsed definition. Returns nothing for the
    // constructor's own lines and for a line no method covers, so both keep
    // resolving against the class name.
    MacroFunctionDef*
    classdefMethodOwningLine(const std::string& className, size_t requestedLine)
    {
        const ClassdefDefinition* definition = classdef_detail::findClassdefDefinition(className);
        if (definition == nullptr) {
            return nullptr;
        }
        for (const auto& method : definition->methods) {
            size_t methodLines = countSourceLines(method.source);
            if (method.sourceStartLine == 0 || methodLines == 0
                || classdef_detail::equalsClassNameOrLeaf(method.name, definition->name)) {
                continue;
            }
            if (requestedLine < method.sourceStartLine
                || requestedLine > method.sourceStartLine + methodLines - 1) {
                continue;
            }
            FunctionDefPtr methodDef = nullptr;
            if (!FunctionsInMemory::getInstance()->find(
                    classdefMethodFunctionName(*definition, method), methodDef,
                    FunctionsInMemory::FIND_FUNCTION_TYPE::MACRO)
                || methodDef == nullptr) {
                return nullptr;
            }
            MacroFunctionDef* methodMacro = static_cast<MacroFunctionDef*>(methodDef);
            return methodMacro->code != nullptr ? methodMacro : nullptr;
        }
        return nullptr;
    }
    //=============================================================================
    bool
    resolveMacroBreakpointCode(MacroFunctionDef* functionDef, const std::wstring& mainFunctionName,
        const std::wstring& subFunctionName, size_t requestedLine, AbstractSyntaxTreePtr& code,
        size_t& maxLinesInFile, std::wstring& errorMessage,
        std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
    {
        if (!subFunctionName.empty()) {
            MacroFunctionDef* targetFunc = findSubFunction(functionDef, subFunctionName);
            if (targetFunc == nullptr) {
                errorMessage
                    = formatI18nMessage(resolveMessageTemplate(L"Nelson:dbstop:subfunctionNotFound",
                                            L"Cannot find subfunction '%s' in '%s'."),
                        subFunctionName, mainFunctionName);
                return false;
            }
            code = targetFunc->code;
            maxLinesInFile = maxLineFromChainHelper(code, getLineFunc);
            return true;
        }
        MacroFunctionDef* targetFunc
            = selectFunctionForLine(functionDef, requestedLine, maxLinesInFile, getLineFunc);
        if (targetFunc == nullptr) {
            errorMessage
                = resolveMessageTemplate(L"Nelson:interpreter:cannotLocateFunctionBreakpoint",
                      L"Cannot locate function for breakpoint at line ")
                + std::to_wstring(requestedLine);
            return false;
        }
        code = targetFunc->code;
        return true;
    }
    //=============================================================================
    bool
    resolveBreakpointCode(Evaluator* evaluator, const std::wstring& mainFunctionName,
        const std::wstring& subFunctionName, const std::wstring& functionName, size_t requestedLine,
        AbstractSyntaxTreePtr& code, size_t& maxLinesInFile, std::wstring& sourceFilePath,
        std::wstring& errorMessage, std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
    {
        FunctionDef* funcDef = nullptr;
        if (evaluator->lookupFunction(wstring_to_utf8(functionName), funcDef)) {
            if (funcDef->type() != NLS_MACRO_FUNCTION) {
                errorMessage
                    = Nelson::resolveMessageTemplate(L"Nelson:interpreter:breakpointsCanOnlySet",
                        L"Breakpoints can only be set in macro functions.");
                return false;
            }
            funcDef->updateCode();
            sourceFilePath = funcDef->getFilename();
            if (subFunctionName.empty()) {
                MacroFunctionDef* methodDef
                    = classdefMethodOwningLine(wstring_to_utf8(functionName), requestedLine);
                if (methodDef != nullptr) {
                    code = methodDef->code;
                    maxLinesInFile = maxLineFromChainHelper(code, getLineFunc);
                    return true;
                }
            }
            return resolveMacroBreakpointCode(static_cast<MacroFunctionDef*>(funcDef),
                mainFunctionName, subFunctionName, requestedLine, code, maxLinesInFile,
                errorMessage, getLineFunc);
        }
        std::wstring resolvedFilename = mainFunctionName;
        std::wstring filenameCopy = mainFunctionName;
        if (PathFunctionIndexerManager::getInstance()->find(
                wstring_to_utf8(mainFunctionName), resolvedFilename)) {
            filenameCopy = resolvedFilename;
        }
        ParserState state = ParseFile(evaluator, filenameCopy);
        if (state != ScriptBlock) {
            errorMessage
                = resolveMessageTemplate(L"Nelson:interpreter:cannotAdjustBreakpointUnable",
                      L"Cannot adjust breakpoint: unable to parse script file '")
                + filenameCopy + L"'.";
            return false;
        }
        sourceFilePath = filenameCopy;
        code = getParsedScriptBlock();
        maxLinesInFile = maxLineFromChainHelper(code, getLineFunc);
        return true;
    }
    //=============================================================================
    bool
    skipCommentOnlyLines(const std::wstring& sourceFilePath, size_t maxLinesInFile,
        size_t& lineToCheck, std::wstring& errorMessage)
    {
        if (sourceFilePath.empty()) {
            return true;
        }
        std::set<size_t> commentOnlyLines = getCommentOnlyLines(sourceFilePath);
        size_t maxTry = maxLinesInFile > 0 ? maxLinesInFile : 10000;
        size_t tryCount = 0;
        while (commentOnlyLines.count(lineToCheck) && tryCount < maxTry) {
            lineToCheck++;
            tryCount++;
        }
        if (commentOnlyLines.count(lineToCheck)) {
            errorMessage
                = resolveMessageTemplate(L"Nelson:interpreter:cannotSetBreakpointExecutable",
                    L"Cannot set breakpoint: no executable statement found.");
            return false;
        }
        return true;
    }
    //=============================================================================
    bool
    skipMultilineCommentLines(Evaluator* evaluator, size_t maxLinesInFile, size_t& lineToCheck,
        std::wstring& errorMessage)
    {
        if (evaluator->lexerContext.textbuffer == nullptr) {
            return true;
        }
        std::wstring src = utf8_to_wstring(evaluator->lexerContext.textbuffer);
        std::set<size_t> commentLines = getMultilineCommentLines(src);
        size_t maxTry = maxLinesInFile > 0 ? maxLinesInFile : 10000;
        size_t tryCount = 0;
        while (commentLines.count(lineToCheck) && tryCount < maxTry) {
            lineToCheck++;
            tryCount++;
        }
        if (commentLines.count(lineToCheck)) {
            errorMessage = resolveMessageTemplate(L"Nelson:interpreter:cannotSetBreakpointInside",
                L"Cannot set breakpoint inside multiline comment block");
            return false;
        }
        return true;
    }
    //=============================================================================
    void
    skipArgumentsBlockLines(Evaluator* evaluator, const std::wstring& mainFunctionName,
        const std::wstring& functionName, size_t maxLinesInFile, std::wstring sourceFilePath,
        size_t& lineToCheck)
    {
        FunctionDef* lookupDef = nullptr;
        if (evaluator->lookupFunction(wstring_to_utf8(functionName), lookupDef)
            && lookupDef != nullptr && !lookupDef->getFilename().empty()) {
            sourceFilePath = lookupDef->getFilename();
        }
        if (sourceFilePath.empty()) {
            sourceFilePath = mainFunctionName;
        }
        std::set<size_t> argBlockLines = getArgumentsBlockLines(sourceFilePath);
        if (argBlockLines.empty() || !argBlockLines.count(lineToCheck)) {
            return;
        }
        size_t candidate = lineToCheck + 1;
        size_t maxTry = maxLinesInFile > 0 ? maxLinesInFile : 10000;
        while (candidate <= maxTry && argBlockLines.count(candidate)) {
            candidate++;
        }
        lineToCheck = candidate;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
Evaluator::adjustBreakpointLine(const std::wstring& filename, size_t requestedLine,
    size_t& adjustedLine, std::wstring& errorMessage)
{
    adjustedLine = 0;
    errorMessage.clear();
    if (filename.empty() || requestedLine == 0) {
        errorMessage
            = resolveMessageTemplate(L"Nelson:interpreter:invalidParametersBreakpointAdjustment",
                L"Invalid parameters for breakpoint adjustment");
        return false;
    }
    auto getLineFunc
        = [this](AbstractSyntaxTreePtr t) -> size_t { return this->getLinePosition(t); };
    AbstractSyntaxTreePtr code = nullptr;
    size_t maxLinesInFile = 0;
    size_t lineToCheck = requestedLine;
    std::wstring mainFunctionName;
    std::wstring subFunctionName;
    std::wstring functionName;
    std::wstring sourceFilePath;
    splitBreakpointName(filename, mainFunctionName, subFunctionName, functionName);
    if (!resolveBreakpointCode(this, mainFunctionName, subFunctionName, functionName, requestedLine,
            code, maxLinesInFile, sourceFilePath, errorMessage, getLineFunc)) {
        return false;
    }
    if (!skipCommentOnlyLines(sourceFilePath, maxLinesInFile, lineToCheck, errorMessage)) {
        return false;
    }
    if (!skipMultilineCommentLines(this, maxLinesInFile, lineToCheck, errorMessage)) {
        return false;
    }
    skipArgumentsBlockLines(
        this, mainFunctionName, functionName, maxLinesInFile, sourceFilePath, lineToCheck);
    adjustedLine = getLineNumberFromCode(code, lineToCheck, maxLinesInFile, getLineFunc);
    if (adjustedLine != 0) {
        return true;
    }
    errorMessage = formatI18nMessage(
        resolveMessageTemplate(L"Nelson:interpreter:cannotSetBreakpointAt",
            L"Cannot set breakpoint at line %d: no executable statement found."),
        requestedLine);
    return false;
}
//=============================================================================
size_t
maxLineFromChainHelper(
    AbstractSyntaxTreePtr t, std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
{
    if (t == nullptr) {
        return 0;
    }

    size_t maxLine = getLineFunc(t);

    if (t->down != nullptr) {
        size_t downMax = maxLineFromChainHelper(t->down, getLineFunc);
        if (downMax > maxLine) {
            maxLine = downMax;
        }
    }

    if (t->right != nullptr) {
        size_t rightMax = maxLineFromChainHelper(t->right, getLineFunc);
        if (rightMax > maxLine) {
            maxLine = rightMax;
        }
    }

    return maxLine;
}
//=============================================================================
namespace {
    //=============================================================================
    bool
    isStatementNode(AbstractSyntaxTreePtr node)
    {
        return node != nullptr && (node->opNum == OP_RSTATEMENT || node->opNum == OP_QSTATEMENT);
    }
    //=============================================================================
    std::vector<std::pair<size_t, size_t>>
    collectStatementRanges(
        AbstractSyntaxTreePtr current, std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
    {
        std::vector<std::pair<size_t, size_t>> ranges;
        AbstractSyntaxTreePtr stmt = current;
        while (stmt != nullptr) {
            size_t startLine = getLineFunc(stmt);
            if (stmt->down != nullptr) {
                size_t downStartLine = getLineFunc(stmt->down);
                if (downStartLine > 0 && downStartLine < startLine) {
                    startLine = downStartLine;
                }
            }
            size_t endLine = startLine;
            if (stmt->down != nullptr) {
                endLine = std::max(endLine, maxLineFromChainHelper(stmt->down, getLineFunc));
            }
            endLine = std::max(endLine, getLineFunc(stmt));
            if (startLine > 0) {
                ranges.push_back(std::make_pair(startLine, endLine));
            }
            stmt = stmt->right;
        }
        return ranges;
    }
    //=============================================================================
    bool
    hasTopLevelRangeStart(const std::vector<std::pair<size_t, size_t>>& ranges, size_t lineNumber)
    {
        for (const auto& range : ranges) {
            if (range.first == lineNumber) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    size_t
    findTopLevelStatementAtLine(AbstractSyntaxTreePtr node, size_t lineNumber,
        const std::vector<std::pair<size_t, size_t>>& ranges,
        std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
    {
        if (node == nullptr) {
            return 0;
        }
        if (getLineFunc(node) == lineNumber && isStatementNode(node)
            && hasTopLevelRangeStart(ranges, lineNumber)) {
            return lineNumber;
        }
        if (node->down != nullptr) {
            size_t result
                = findTopLevelStatementAtLine(node->down, lineNumber, ranges, getLineFunc);
            if (result > 0) {
                return result;
            }
        }
        if (node->right != nullptr) {
            return findTopLevelStatementAtLine(node->right, lineNumber, ranges, getLineFunc);
        }
        return 0;
    }
    //=============================================================================
    size_t
    findStatementInSubtree(AbstractSyntaxTreePtr node, size_t lineNumber,
        std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
    {
        if (node == nullptr) {
            return 0;
        }
        if (getLineFunc(node) == lineNumber && isStatementNode(node)) {
            return lineNumber;
        }
        if (node->down != nullptr) {
            size_t result = findStatementInSubtree(node->down, lineNumber, getLineFunc);
            if (result > 0) {
                return result;
            }
        }
        if (node->right != nullptr) {
            return findStatementInSubtree(node->right, lineNumber, getLineFunc);
        }
        return 0;
    }
    //=============================================================================
    size_t
    findLineInStatementRanges(
        const std::vector<std::pair<size_t, size_t>>& ranges, size_t lineNumber)
    {
        for (const auto& range : ranges) {
            size_t stmtStart = range.first;
            size_t stmtEnd = range.second;
            if (lineNumber >= stmtStart && lineNumber <= stmtEnd) {
                if (stmtEnd - stmtStart > 5) {
                    return lineNumber;
                }
                return (stmtStart < stmtEnd && lineNumber > stmtStart) ? stmtEnd : lineNumber;
            }
        }
        for (const auto& range : ranges) {
            if (range.first > lineNumber) {
                return range.first;
            }
        }
        return 0;
    }
    //=============================================================================
} // namespace
//=============================================================================
size_t
getLineNumberFromCode(AbstractSyntaxTreePtr code, size_t lineNumber, size_t maxLinesInFile,
    std::function<size_t(AbstractSyntaxTreePtr)> getLineFunc)
{
    if (code == nullptr || lineNumber == 0) {
        return 0;
    }
    AbstractSyntaxTreePtr current = code;
    if (code->opNum == OP_BLOCK && code->down != nullptr) {
        current = code->down;
    }
    auto statementRanges = collectStatementRanges(current, getLineFunc);
    if (statementRanges.empty()) {
        return 0;
    }
    size_t foundLine
        = findTopLevelStatementAtLine(current, lineNumber, statementRanges, getLineFunc);
    if (foundLine > 0) {
        return foundLine;
    }
    foundLine = findStatementInSubtree(code, lineNumber, getLineFunc);
    if (foundLine > 0) {
        return foundLine;
    }
    foundLine = findLineInStatementRanges(statementRanges, lineNumber);
    if (foundLine > 0) {
        return foundLine;
    }
    if (lineNumber <= maxLinesInFile) {
        return statementRanges.back().first;
    }
    return 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
