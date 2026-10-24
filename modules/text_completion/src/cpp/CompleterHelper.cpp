//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CompleterHelper.hpp"
#include "StringHelpers.hpp"
#include "IsValidVariableName.hpp"
#include "Context.hpp"
#include "Scope.hpp"
#include "BuiltinCompleter.hpp"
#include "FileCompleter.hpp"
#include "Keywords.hpp"
#include "MacroCompleter.hpp"
#include "VariableCompleter.hpp"
#include "FieldCompleter.hpp"
#include "MethodCompleter.hpp"
#include "PropertyCompleter.hpp"
#include "EvaluateCommand.hpp"
#include "ArrayOf.hpp"
#include "HandleState.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include <algorithm>
#include <string>
#include <unordered_set>
//=============================================================================
namespace Nelson {
//=============================================================================
static thread_local Evaluator* completionEvaluatorOverride = nullptr;
//=============================================================================
class CompletionEvaluatorScope
{
private:
    Evaluator* previousEvaluator = nullptr;

public:
    explicit CompletionEvaluatorScope(Evaluator* eval)
    {
        previousEvaluator = completionEvaluatorOverride;
        completionEvaluatorOverride = eval;
    }

    ~CompletionEvaluatorScope() { completionEvaluatorOverride = previousEvaluator; }
};
//=============================================================================
Evaluator*
getCompletionEvaluator()
{
    if (completionEvaluatorOverride != nullptr) {
        return completionEvaluatorOverride;
    }
    return static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
}
//=============================================================================
bool
isValidCompletionHandle(const ArrayOf& value)
{
    if (!value.isHandle()) {
        return true;
    }
    const auto* refs = static_cast<const HandleRef*>(value.dataPointer());
    if (refs == nullptr) {
        return false;
    }
    for (indexType k = 0; k < value.elementCount(); ++k) {
        if (refs[k] == nullptr || !refs[k]->isValid()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
// Characters that end an identifier-like completion token; '.' is part of the
// list so the token is always the leaf after the last dot.
static const std::wstring COMPLETION_SYMBOLS = L"+-*/\\([ ^,;={.&|\'])}:'><~@\t";
//=============================================================================
static inline bool
isCharacterInSymbols(wchar_t character, const std::wstring& symbols)
{
    return std::find(symbols.begin(), symbols.end(), character) != symbols.end();
}
//=============================================================================
static bool
hasPathSyntax(const std::wstring& text)
{
    if (text.find(L'/') != std::wstring::npos || text.find(L'\\') != std::wstring::npos) {
        return true;
    }
    if (text.size() >= 2 && text[1] == L':') {
        return true;
    }
    return StringHelpers::starts_with(text, L"./") || StringHelpers::starts_with(text, L"../");
}
//=============================================================================
static bool
isFileCommand(const std::wstring& command)
{
    static const wstringVector fileCommands = { L"addpath", L"cd", L"chdir", L"copyfile", L"delete",
        L"diary", L"dir", L"edit", L"editor", L"exist", L"filewrite", L"fileread", L"fopen",
        L"genpath", L"isdir", L"isfile", L"load", L"ls", L"mkdir", L"movefile", L"open", L"path",
        L"rmfile", L"rmdir", L"rmpath", L"run", L"save", L"type", L"unzip", L"what", L"zip" };

    std::wstring commandLower = command;
    StringHelpers::to_lower(commandLower);
    return std::find(fileCommands.begin(), fileCommands.end(), commandLower) != fileCommands.end();
}
//=============================================================================
static std::wstring
getCompletionCommandName(const std::wstring& line, const std::wstring& completionPart)
{
    std::wstring trimmedLine = line;
    StringHelpers::trim_left(trimmedLine);
    if (trimmedLine.empty() || completionPart.empty()) {
        return {};
    }

    size_t completionPartPosition = trimmedLine.rfind(completionPart);
    if (completionPartPosition == std::wstring::npos || completionPartPosition == 0) {
        return {};
    }

    std::wstring beforeCompletionPart = trimmedLine.substr(0, completionPartPosition);
    StringHelpers::trim_right(beforeCompletionPart);
    size_t commandEnd = beforeCompletionPart.find_first_of(L" \t('\"");
    std::wstring command = commandEnd == std::wstring::npos
        ? beforeCompletionPart
        : beforeCompletionPart.substr(0, commandEnd);
    StringHelpers::to_lower(command);
    return command;
}
//=============================================================================
static bool
isFileCommandContext(const std::wstring& line, const std::wstring& filepart)
{
    return isFileCommand(getCompletionCommandName(line, filepart));
}
//=============================================================================
static bool
isEditMacroCompletionContext(
    const std::wstring& line, const std::wstring& filepart, const std::wstring& textpart)
{
    if (filepart.empty() || textpart.empty() || filepart != textpart || hasPathSyntax(filepart)) {
        return false;
    }
    return getCompletionCommandName(line, filepart) == L"edit";
}
//=============================================================================
static bool
isFileCompletionContext(
    const std::wstring& line, const std::wstring& filepart, const std::wstring& textpart)
{
    if (filepart.empty()) {
        return false;
    }
    return hasPathSyntax(filepart) || isFileCommandContext(line, filepart)
        || (!textpart.empty()
            && (filepart[0] == L'\'' || filepart[0] == L'"' || textpart[0] == L'\''
                || textpart[0] == L'"'));
}
//=============================================================================
// Matching is case-insensitive: the completion entry replaces the typed token,
// which corrects its case on insert.
static int
completionMatchRank(const std::wstring& value, const std::wstring& prefix)
{
    if (prefix.empty()) {
        return 1;
    }
    if (StringHelpers::iequals(value, prefix)) {
        return 0;
    }
    if (StringHelpers::istarts_with(value, prefix)) {
        return 1;
    }
    size_t lastDot = value.rfind(L'.');
    if (lastDot != std::wstring::npos && lastDot + 1 < value.size()
        && StringHelpers::istarts_with(value.substr(lastDot + 1), prefix)) {
        return 2;
    }
    return 3;
}
//=============================================================================
static void
sortAndUniqueCompletions(wstringVector& values, const std::wstring& prefix)
{
    std::sort(values.begin(), values.end(),
        [&prefix](const std::wstring& left, const std::wstring& right) {
            int leftRank = completionMatchRank(left, prefix);
            int rightRank = completionMatchRank(right, prefix);
            if (leftRank != rightRank) {
                return leftRank < rightRank;
            }
            if (left.size() != right.size()) {
                return left.size() < right.size();
            }
            return left < right;
        });
    values.erase(std::unique(values.begin(), values.end()), values.end());
}
//=============================================================================
static void
removeExistingNames(wstringVector& values, const std::unordered_set<std::wstring>& names)
{
    values.erase(std::remove_if(values.begin(), values.end(),
                     [&names](const std::wstring& value) { return names.count(value) != 0; }),
        values.end());
}
//=============================================================================
static void
removeFunctionDuplicates(
    wstringVector& builtin, wstringVector& macros, const wstringVector& variables)
{
    std::unordered_set<std::wstring> variableNames(variables.begin(), variables.end());
    removeExistingNames(macros, variableNames);
    removeExistingNames(builtin, variableNames);

    std::unordered_set<std::wstring> macroNames(macros.begin(), macros.end());
    removeExistingNames(builtin, macroNames);
}
//=============================================================================
static void
keepMatchingCompletions(wstringVector& values, const std::wstring& prefix)
{
    values.erase(std::remove_if(values.begin(), values.end(),
                     [&prefix](const std::wstring& value) {
                         return completionMatchRank(value, prefix) == 3;
                     }),
        values.end());
}
//=============================================================================
// Returns the dotted container typed right before the completion token, dot
// included (line "x = nelson.u", tokenStart on 'u' gives "nelson.").
static std::wstring
getMemberContainer(const std::wstring& line, size_t tokenStart)
{
    size_t start = tokenStart;
    while (start > 0) {
        wchar_t ch = line[start - 1];
        if (ch != L'.' && isCharacterInSymbols(ch, COMPLETION_SYMBOLS)) {
            break;
        }
        start--;
    }
    return line.substr(start, tokenStart - start);
}
//=============================================================================
// True when container looks like a dotted package or class path ("nelson." or
// "nelson.lang."), the only shape BuiltinCompleter/MacroCompleter can resolve.
static bool
isPackageLikeContainer(const std::wstring& container)
{
    if (container.size() < 2 || container.front() == L'.' || container.back() != L'.') {
        return false;
    }
    return container.find(L"..") == std::wstring::npos;
}
//=============================================================================
// Qualified candidates ("nelson.unittest") become replacements for the leaf
// token: entries living in the typed container lose that container, the
// others are dropped.
static void
keepAndStripContainer(wstringVector& values, const std::wstring& container)
{
    wstringVector kept;
    kept.reserve(values.size());
    for (const auto& value : values) {
        if (value.size() > container.size() && StringHelpers::starts_with(value, container)) {
            kept.emplace_back(value.substr(container.size()));
        }
    }
    values = std::move(kept);
}
//=============================================================================
static void
appendImmediatePackageCompletions(wstringVector& macros, const std::wstring& lookupPrefix)
{
    size_t lastDot = lookupPrefix.rfind(L'.');
    if (lastDot == std::wstring::npos) {
        return;
    }

    const std::wstring container = lookupPrefix.substr(0, lastDot + 1);
    const std::wstring typedLeaf = lookupPrefix.substr(lastDot + 1);
    const wstringVector indexedMacros = macros;
    for (const auto& macro : indexedMacros) {
        if (!StringHelpers::starts_with(macro, container) || macro.size() <= container.size()) {
            continue;
        }

        const std::wstring relativeName = macro.substr(container.size());
        size_t nextDot = relativeName.find(L'.');
        if (nextDot == std::wstring::npos) {
            continue;
        }

        const std::wstring immediateName = relativeName.substr(0, nextDot);
        if (!StringHelpers::istarts_with(immediateName, typedLeaf)) {
            continue;
        }
        macros.emplace_back(container + immediateName);
    }
}
//=============================================================================
std::wstring
getPartialLine(const std::wstring& line)
{
    const std::wstring& symbols = COMPLETION_SYMBOLS;
    size_t index = std::wstring::npos;
    for (wchar_t symbol : symbols) {
        size_t pch = line.rfind(symbol);
        if (pch != std::wstring::npos) {
            if (index == std::wstring::npos) {
                index = pch;
            } else {
                index = std::max(index, pch);
            }
        }
    }

    if (index != std::wstring::npos && index + 1 < line.length()) {
        std::wstring prefix = line.substr(0, index);
        if (!IsValidVariableName(prefix) || isCharacterInSymbols(line[index], symbols)) {
            return line.substr(index + 1);
        }
    }
    return line;
}
//=============================================================================
std::string
getLookupSymbolFromCompletionExpression(const std::string& expression)
{
    std::string symbol = expression;
    size_t lastCharacterIndex = symbol.find_last_of(" \t,;");
    if (lastCharacterIndex != std::string::npos) {
        symbol = symbol.substr(lastCharacterIndex + 1);
    }

    size_t indexingStart = symbol.find_first_of("([{");
    if (indexingStart != std::string::npos) {
        symbol = symbol.substr(0, indexingStart);
    }
    return symbol;
}
//=============================================================================
static std::string
getRootSymbolFromCompletionExpression(const std::string& expression)
{
    std::string symbol = expression;
    size_t lastCharacterIndex = symbol.find_last_of(" \t,;");
    if (lastCharacterIndex != std::string::npos) {
        symbol = symbol.substr(lastCharacterIndex + 1);
    }

    size_t rootEnd = symbol.find_first_not_of(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_");
    if (rootEnd != std::string::npos) {
        symbol = symbol.substr(0, rootEnd);
    }
    return symbol;
}
//=============================================================================
static bool
lookupCompletionVariableInScope(Scope* scope, const std::string& variableName, ArrayOf& value)
{
    return scope != nullptr && scope->lookupVariable(variableName, value);
}
//=============================================================================
bool
lookupCompletionVariable(Context* context, const std::string& variableName, ArrayOf& value)
{
    if (context == nullptr) {
        return false;
    }
    Scope* currentScope = context->getCurrentScope();
    if (lookupCompletionVariableInScope(currentScope, variableName, value)) {
        return true;
    }
    Scope* callerScope = context->getCallerScope();
    if (callerScope != currentScope
        && lookupCompletionVariableInScope(callerScope, variableName, value)) {
        return true;
    }
    Scope* baseScope = context->getBaseScope();
    if (baseScope != currentScope && baseScope != callerScope
        && lookupCompletionVariableInScope(baseScope, variableName, value)) {
        return true;
    }
    Scope* globalScope = context->getGlobalScope();
    return globalScope != currentScope && globalScope != callerScope && globalScope != baseScope
        && lookupCompletionVariableInScope(globalScope, variableName, value);
}
//=============================================================================
bool
resolveCompletionExpressionValue(
    Context* context, Evaluator* eval, const std::string& expression, ArrayOf& value)
{
    if (lookupCompletionVariable(context, expression, value)) {
        return true;
    }

    std::string rootSymbol = getRootSymbolFromCompletionExpression(expression);
    if (rootSymbol.empty()) {
        return false;
    }

    ArrayOf rootValue;
    if (rootSymbol != expression && !lookupCompletionVariable(context, rootSymbol, rootValue)) {
        return false;
    }

    ArrayOfVector variableArray
        = EvaluateCommand(eval, 1, utf8_to_wstring(expression), std::wstring());
    if (variableArray.size() != 1) {
        return false;
    }
    value = variableArray[0];
    return true;
}
//=============================================================================
std::wstring
getPartialLineAsPath(const std::wstring& line)
{
    if (line.empty()) {
        return {};
    }
    std::wstring symbols = std::wstring(L";,'");
    std::wstring lineWithoutSpaceAtBeginning = line;
    StringHelpers::trim_left(lineWithoutSpaceAtBeginning);
    if (lineWithoutSpaceAtBeginning.empty()) {
        return {};
    }
    size_t lengthLine = line.size();
    std::wstring returnedLine;
    bool symbol_found = false;
    size_t index = 0;
    // cd c:/Program Files (x86)/
    for (wchar_t symbol : symbols) {
        std::string::size_type filePos = lineWithoutSpaceAtBeginning.rfind(symbol);
        if (filePos != std::wstring::npos) {
            index = std::max(index, filePos);
            symbol_found = true;
            break;
        }
    }
    if (!symbol_found) {
        // example: cd c:/
        size_t lastPositionBlank = lineWithoutSpaceAtBeginning.rfind(L' ');
        size_t firstPositionBlank = lineWithoutSpaceAtBeginning.find(L' ');
        if (lastPositionBlank != std::wstring::npos && firstPositionBlank != std::wstring::npos) {
            symbol_found = true;
            index = firstPositionBlank;
        }
    }
    if (symbol_found) {
        index++;
    }
    while (lineWithoutSpaceAtBeginning[index] == L' ') {
        if (index + 1 >= lengthLine) {
            break;
        }
        index++;
    }
    returnedLine = lineWithoutSpaceAtBeginning.substr(index);

    return returnedLine;
}
//=============================================================================
std::wstring
getCompletionLeafPrefix(const std::wstring& completionPrefix)
{
    size_t lastSep = completionPrefix.find_last_of(L"/\\");
    if (lastSep == std::wstring::npos) {
        return completionPrefix;
    }
    return completionPrefix.substr(lastSep + 1);
}
//=============================================================================
std::wstring
getCompletionTokenToReplace(const std::wstring& line, bool isPathCompletion)
{
    if (line.empty()) {
        return {};
    }
    if (isPathCompletion) {
        return getCompletionLeafPrefix(getPartialLineAsPath(line));
    }
    size_t index = line.find_last_of(COMPLETION_SYMBOLS);
    if (index == std::wstring::npos) {
        return line;
    }
    return line.substr(index + 1);
}
//=============================================================================
std::wstring
completerLine(
    const std::wstring& currentLine, const std::wstring& stringToAdd, bool stringToAddIsPath)
{
    if (stringToAdd.empty()) {
        return currentLine;
    }
    std::wstring token = getCompletionTokenToReplace(currentLine, stringToAddIsPath);
    if (!token.empty() && StringHelpers::ends_with(currentLine, token)) {
        return currentLine.substr(0, currentLine.size() - token.size()) + stringToAdd;
    }
    return currentLine + stringToAdd;
}
//=============================================================================
bool
computeCompletion(const std::wstring& line, std::wstring& completionPrefix, wstringVector& files,
    wstringVector& builtin, wstringVector& macros, wstringVector& variables, wstringVector& fields,
    wstringVector& properties, wstringVector& methods)
{
    bool showpopup = false;
    completionPrefix = line;
    std::wstring filepart = getPartialLineAsPath(line);
    std::wstring textpart = getPartialLine(line);
    if (isFileCompletionContext(line, filepart, textpart)) {
        std::wstring leafPrefix = getCompletionLeafPrefix(filepart);
        files = FileCompleter(filepart);
        if (isEditMacroCompletionContext(line, filepart, textpart)) {
            macros = MacroCompleter(textpart);
            keepMatchingCompletions(macros, leafPrefix);
            sortAndUniqueCompletions(macros, leafPrefix);
        }
        if (!files.empty() || !macros.empty()) {
            completionPrefix = leafPrefix;
            showpopup = true;
        }
    } else if (!textpart.empty()) {
        std::wstring token = getCompletionTokenToReplace(line, false);
        size_t tokenStart = line.size() - token.size();
        bool memberContext = (tokenStart > 0) && (line[tokenStart - 1] == L'.');

        if (memberContext) {
            fields = FieldCompleter(line);
            properties = PropertyCompleter(line);
            methods = MethodCompleter(line);

            std::wstring container = getMemberContainer(line, tokenStart);
            if (isPackageLikeContainer(container)) {
                std::wstring memberLookup = container + token;
                builtin = BuiltinCompleter(memberLookup);
                macros = MacroCompleter(memberLookup);
                appendImmediatePackageCompletions(macros, memberLookup);
                keepAndStripContainer(builtin, container);
                keepAndStripContainer(macros, container);
            }
        } else if (!token.empty()) {
            builtin = BuiltinCompleter(token);
            // Language keywords ride along with builtins; the shared ranking
            // and deduplication below apply to them too.
            for (const auto& keyword : GetKeywords(false)) {
                if (StringHelpers::istarts_with(keyword, token)) {
                    builtin.push_back(keyword);
                }
            }
            macros = MacroCompleter(token);
            variables = VariableCompleter(token);

            keepMatchingCompletions(variables, token);
            sortAndUniqueCompletions(variables, token);
        }

        keepMatchingCompletions(builtin, token);
        keepMatchingCompletions(macros, token);
        sortAndUniqueCompletions(macros, token);
        sortAndUniqueCompletions(builtin, token);
        removeFunctionDuplicates(builtin, macros, variables);

        keepMatchingCompletions(fields, token);
        keepMatchingCompletions(properties, token);
        keepMatchingCompletions(methods, token);
        sortAndUniqueCompletions(fields, token);
        sortAndUniqueCompletions(properties, token);
        sortAndUniqueCompletions(methods, token);

        if (!builtin.empty() || !macros.empty() || !variables.empty() || !fields.empty()
            || !properties.empty() || !methods.empty()) {
            completionPrefix = token;
            showpopup = true;
        }
    }
    return showpopup;
}
//=============================================================================
bool
computeCompletion(Evaluator* eval, const std::wstring& line, std::wstring& completionPrefix,
    wstringVector& files, wstringVector& builtin, wstringVector& macros, wstringVector& variables,
    wstringVector& fields, wstringVector& properties, wstringVector& methods)
{
    CompletionEvaluatorScope scope(eval);
    return computeCompletion(
        line, completionPrefix, files, builtin, macros, variables, fields, properties, methods);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
