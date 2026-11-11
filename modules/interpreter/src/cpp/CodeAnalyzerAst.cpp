//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerAst.hpp"
#include "CodeAnalyzerAstUtilities.hpp"
#include "CodeAnalyzerFunctionModel.hpp"
#include "Keywords.hpp"
#include "LexerContext.hpp"
#include <algorithm>
#include <set>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    using namespace CodeAnalyzerAst;
    //=============================================================================
    struct AstSourceSpan
    {
        int line = 0;
        int column = 0;
    };
    //=============================================================================
    AstSourceSpan
    spanFromContext(int context)
    {
        AstSourceSpan span;
        // Nodes made by the parser without a source position use -1.
        if (context < 0) {
            return span;
        }
        span.line = context & 0x0000FFFF;
        span.column = (context >> 16) & 0xFFFF;
        return span;
    }
    //=============================================================================
    AstSourceSpan
    firstSourceSpan(AbstractSyntaxTreePtr node)
    {
        if (node == nullptr) {
            return {};
        }
        AstSourceSpan span = spanFromContext(node->getContext());
        if (span.line > 0) {
            return span;
        }
        span = firstSourceSpan(node->down);
        if (span.line > 0) {
            return span;
        }
        return firstSourceSpan(node->right);
    }
    //=============================================================================
    CodeAnalyzerLineUsage*
    usageAt(std::vector<CodeAnalyzerLineUsage>& lineUsages, int lineNumber)
    {
        if (lineNumber <= 0 || lineNumber > static_cast<int>(lineUsages.size())) {
            return nullptr;
        }
        return &lineUsages[static_cast<size_t>(lineNumber - 1)];
    }
    //=============================================================================
    void
    appendUnique(stringVector& values, const std::string& value)
    {
        if (value.empty() || std::find(values.begin(), values.end(), value) != values.end()) {
            return;
        }
        values.push_back(value);
    }
    //=============================================================================
    AbstractSyntaxTreePtr
    unwrapRhs(AbstractSyntaxTreePtr node)
    {
        while (node != nullptr && node->type == non_terminal && node->opNum == OP_RHS) {
            node = node->down;
        }
        return node;
    }
    //=============================================================================
    AbstractSyntaxTreePtr
    rightOperand(AbstractSyntaxTreePtr node)
    {
        return node != nullptr && node->down != nullptr ? node->down->right : nullptr;
    }
    //=============================================================================
    bool
    isTrackedIdentifier(const std::string& name)
    {
        if (name.empty() || name == "~") {
            return false;
        }
        // The parser names each '~' output with this prefix.
        if (name.compare(0, UNUSED_PLACEHOLDER_PREFIX_LEN, UNUSED_PLACEHOLDER_PREFIX) == 0) {
            return false;
        }
        // 'arguments' is a keyword only at the start of a block, so it can name a variable.
        if (name == "arguments") {
            return true;
        }
        const std::set<std::string>& reserved = reservedWords();
        return reserved.find(toLowerAscii(name)) == reserved.end();
    }
    //=============================================================================
    void
    prepareAstUsage(CodeAnalyzerLineUsage& usage)
    {
        if (usage.usageFromAst) {
            return;
        }
        usage.used.clear();
        usage.callableUsed.clear();
        usage.usageFromAst = true;
    }
    //=============================================================================
    void
    appendAstUsage(
        std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node, bool callable)
    {
        if (node == nullptr || node->type != id_node || !isTrackedIdentifier(node->text)) {
            return;
        }
        AstSourceSpan span = firstSourceSpan(node);
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        prepareAstUsage(*usage);
        // A call to a variable assigned earlier on the same line reads that variable.
        if (callable && usage->assignmentFromAst
            && std::find(usage->assigned.begin(), usage->assigned.end(), node->text)
                != usage->assigned.end()) {
            callable = false;
        }
        appendUnique(callable ? usage->callableUsed : usage->used, node->text);
    }
    //=============================================================================
    void
    markExpressionUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node,
        const std::set<std::string>& hidden = {});
    //=============================================================================
    void
    markPostfixUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr postfix,
        const std::set<std::string>& hidden = {})
    {
        for (AbstractSyntaxTreePtr current = postfix; current != nullptr;
            current = current->right) {
            if (current->opNum == OP_PARENS || current->opNum == OP_BRACES
                || current->opNum == OP_DOTDYN) {
                markExpressionUsage(lineUsages, current->down, hidden);
            }
        }
    }
    //=============================================================================
    void
    markAnonymousFunctionUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages,
        AbstractSyntaxTreePtr node, std::set<std::string> hidden)
    {
        // With parameters, the first one is 'down', the others its children, the body its peer.
        AbstractSyntaxTreePtr body = node->down;
        if (body != nullptr && body->right != nullptr) {
            hidden.insert(body->text);
            for (AbstractSyntaxTreePtr arg = body->down; arg != nullptr; arg = arg->right) {
                hidden.insert(arg->text);
            }
            body = body->right;
        }
        markExpressionUsage(lineUsages, body, hidden);
    }
    //=============================================================================
    // Marks one expression node, without its right peers.
    void
    markExpressionNodeUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages,
        AbstractSyntaxTreePtr current, const std::set<std::string>& hidden = {})
    {
        if (current == nullptr) {
            return;
        }
        AbstractSyntaxTreePtr expression = unwrapRhs(current);
        if (expression != current) {
            markExpressionUsage(lineUsages, expression, hidden);
            return;
        }
        if (current->type == id_node) {
            const bool parenReference
                = current->down != nullptr && current->down->opNum == OP_PARENS;
            if (hidden.find(current->text) == hidden.end()) {
                appendAstUsage(lineUsages, current, parenReference);
            }
            markPostfixUsage(lineUsages, current->down, hidden);
            return;
        }
        if (current->type != non_terminal || current->opNum == OP_DOT) {
            return;
        }
        if (current->opNum == OP_FUNCTION_HANDLE_ANONYMOUS) {
            markAnonymousFunctionUsage(lineUsages, current, hidden);
            return;
        }
        // @name refers to the function 'name'.
        if (current->opNum == OP_FUNCTION_HANDLE_NAMED) {
            appendAstUsage(lineUsages, current->down, true);
            return;
        }
        markExpressionUsage(lineUsages, current->down, hidden);
    }
    //=============================================================================
    void
    markExpressionUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node,
        const std::set<std::string>& hidden)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            markExpressionNodeUsage(lineUsages, current, hidden);
        }
    }
    //=============================================================================
    bool
    isNamedParenCall(AbstractSyntaxTreePtr node, const std::string& name)
    {
        node = unwrapRhs(node);
        return node != nullptr && node->type == id_node && node->text == name
            && node->down != nullptr && node->down->opNum == OP_PARENS;
    }
    //=============================================================================
    bool
    firstArgumentIsSprintf(AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != id_node || node->down == nullptr
            || node->down->opNum != OP_PARENS) {
            return false;
        }
        return isNamedParenCall(node->down->down, "sprintf");
    }
    //=============================================================================
    bool
    floatingLiteralText(const std::string& text)
    {
        return text.find('.') != std::string::npos || text.find('e') != std::string::npos
            || text.find('E') != std::string::npos;
    }
    //=============================================================================
    bool
    isConstantConditionLiteral(AbstractSyntaxTreePtr node)
    {
        node = unwrapRhs(node);
        if (node == nullptr) {
            return false;
        }
        if (node->type == id_node) {
            return node->text == "true" || node->text == "false";
        }
        if (node->type == const_int_node || node->type == const_double_node
            || node->type == const_float_node) {
            return node->text == "1" || node->text == "0";
        }
        return false;
    }
    //=============================================================================
    void
    markConstantCondition(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr condition)
    {
        if (!isConstantConditionLiteral(condition)) {
            return;
        }
        condition = unwrapRhs(condition);
        AstSourceSpan span = spanFromContext(condition->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        usage->constantCondition = true;
        usage->constantConditionColumn = span.column > 0
            ? span.column
            : codeAnalyzerWordColumnInLine(
                  lines[static_cast<size_t>(span.line - 1)], condition->text);
    }
    //=============================================================================
    void
    markIfConstantConditions(std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const stringVector& lines, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->opNum != OP_CSTAT) {
            return;
        }
        markConstantCondition(lineUsages, lines, node->down);
        AbstractSyntaxTreePtr elseOrElseIf = node->right;
        if (elseOrElseIf == nullptr || elseOrElseIf->opNum != OP_ELSEIFBLOCK) {
            return;
        }
        for (AbstractSyntaxTreePtr current = elseOrElseIf->down; current != nullptr;
            current = current->right) {
            if (current->opNum == OP_CSTAT) {
                markConstantCondition(lineUsages, lines, current->down);
            }
        }
    }
    //=============================================================================
    bool
    subtreeHasFloatingLiteral(AbstractSyntaxTreePtr node)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            if ((current->type == const_double_node || current->type == const_float_node)
                && floatingLiteralText(current->text)) {
                return true;
            }
            if (current->down != nullptr && subtreeHasFloatingLiteral(current->down)) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    int
    operatorColumnInLine(const stringVector& lines, int lineNumber, OP_TYPE opNum)
    {
        if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
            return 1;
        }
        const std::string code = codeWithoutStringsAndComments(lines[lineNumber - 1]);
        const std::string token = opNum == OP_NEQ ? "~=" : "==";
        const size_t position = code.find(token);
        return position == std::string::npos ? 1 : static_cast<int>(position + 1);
    }
    //=============================================================================
    bool
    hasAssignmentBeforeColumn(const stringVector& lines, int lineNumber, int column)
    {
        if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size()) || column <= 1) {
            return false;
        }
        const std::string code
            = codeWithoutStringsAndComments(lines[static_cast<size_t>(lineNumber - 1)]);
        const size_t limit = std::min(static_cast<size_t>(column - 1), code.size());
        for (size_t k = 0; k < limit; ++k) {
            if (code[k] != '=') {
                continue;
            }
            const bool previousComparison = k > 0
                && (code[k - 1] == '<' || code[k - 1] == '>' || code[k - 1] == '~'
                    || code[k - 1] == '=');
            const bool nextComparison = k + 1 < code.size() && code[k + 1] == '=';
            if (!previousComparison && !nextComparison) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    bool
    isUnsafeLoadAstCall(const stringVector& lines, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != id_node || node->text != "load"
            || node->down == nullptr || node->down->opNum != OP_PARENS) {
            return false;
        }
        AstSourceSpan span = spanFromContext(node->getContext());
        if (span.line <= 0 || span.line > static_cast<int>(lines.size())) {
            return false;
        }
        const int column
            = codeAnalyzerWordColumnInLine(lines[static_cast<size_t>(span.line - 1)], "load");
        return !hasAssignmentBeforeColumn(lines, span.line, column);
    }
    //=============================================================================
    bool
    isUnsafeLoadCommandAstCall(const stringVector& lines, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != id_node || node->text != "load"
            || node->down != nullptr) {
            return false;
        }
        AstSourceSpan span = spanFromContext(node->getContext());
        if (span.line <= 0 || span.line > static_cast<int>(lines.size())) {
            return false;
        }
        const std::string code
            = codeWithoutStringsAndComments(lines[static_cast<size_t>(span.line - 1)]);
        const std::string lower = toLowerAscii(trim(code));
        if (!startsWithWord(lower, "load")) {
            return false;
        }
        size_t cursor = 4;
        while (cursor < lower.size() && std::isspace(static_cast<unsigned char>(lower[cursor]))) {
            ++cursor;
        }
        if (cursor < lower.size() && lower[cursor] == '(') {
            return false;
        }
        const int column
            = codeAnalyzerWordColumnInLine(lines[static_cast<size_t>(span.line - 1)], "load");
        return !hasAssignmentBeforeColumn(lines, span.line, column);
    }
    //=============================================================================
    bool
    endsWithContinuationMarker(const stringVector& lines, int lineNumber)
    {
        if (lineNumber <= 0 || lineNumber > static_cast<int>(lines.size())) {
            return false;
        }
        std::string code
            = trim(codeWithoutStringsAndComments(lines[static_cast<size_t>(lineNumber - 1)]));
        return code.size() >= 3 && code.compare(code.size() - 3, 3, "...") == 0;
    }
    //=============================================================================
    int
    logicalStatementEndLine(const stringVector& lines, int lineNumber)
    {
        int endLine = lineNumber;
        while (endLine > 0 && endLine < static_cast<int>(lines.size())
            && endsWithContinuationMarker(lines, endLine)) {
            ++endLine;
        }
        return endLine;
    }
    //=============================================================================
    void
    markImportLineRange(std::vector<CodeAnalyzerLineUsage>& lineUsages, int firstLine, int lastLine)
    {
        for (int line = firstLine; line <= lastLine; ++line) {
            CodeAnalyzerLineUsage* usage = usageAt(lineUsages, line);
            if (usage == nullptr) {
                continue;
            }
            usage->importLine = true;
            usage->importFromAst = true;
        }
    }
    //=============================================================================
    std::string
    importedSymbolName(const std::string& spec)
    {
        size_t dot = spec.find_last_of('.');
        return dot == std::string::npos ? spec : spec.substr(dot + 1);
    }
    //=============================================================================
    std::string
    staticDottedNameFromAst(AbstractSyntaxTreePtr node)
    {
        if (node == nullptr) {
            return "";
        }
        node = unwrapRhs(node);
        if (node == nullptr) {
            return "";
        }
        if (node->type == id_node) {
            std::string name = node->text;
            for (AbstractSyntaxTreePtr sub = node->down; sub != nullptr; sub = sub->right) {
                if (sub->opNum != OP_DOT || sub->down == nullptr || sub->down->type != id_node) {
                    return "";
                }
                name += ".";
                name += sub->down->text;
            }
            return name;
        }
        return "";
    }
    //=============================================================================
    void
    appendImportedSpec(CodeAnalyzerLineUsage& usage, const std::string& spec)
    {
        const std::string name = importedSymbolName(spec);
        if (name == "*") {
            usage.wildcardImport = true;
        } else if (!name.empty()) {
            appendUnique(usage.importedNames, name);
        }
        if (!name.empty()) {
            appendUnique(usage.importedSpecs, spec);
        }
    }
    //=============================================================================
    void
    collectAssignmentTarget(std::vector<CodeAnalyzerLineUsage>& lineUsages,
        CodeAnalyzerLineUsage& usage, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr) {
            return;
        }
        if (node->type == id_node) {
            if (isTrackedIdentifier(node->text)) {
                appendUnique(usage.assigned, node->text);
                if (node->down != nullptr) {
                    appendUnique(usage.leftModified, node->text);
                    markPostfixUsage(lineUsages, node->down);
                }
            }
            return;
        }
        if (node->type == non_terminal && node->opNum == OP_DOT) {
            return;
        }
        collectAssignmentTarget(lineUsages, usage, node->down);
        collectAssignmentTarget(lineUsages, usage, node->right);
    }
    //=============================================================================
    void
    markAssignment(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->opNum != OP_ASSIGN || node->down == nullptr) {
            return;
        }
        AbstractSyntaxTreePtr lhs = node->down;
        AbstractSyntaxTreePtr rhs = rightOperand(node);
        AstSourceSpan span = spanFromContext(node->getContext());
        if (span.line <= 0) {
            span = firstSourceSpan(lhs);
        }
        AstSourceSpan rhsSpan = firstSourceSpan(rhs);
        if (rhsSpan.line > span.line) {
            span.line = rhsSpan.line;
        }
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        if (!usage->assignmentFromAst) {
            usage->assigned.clear();
            usage->leftModified.clear();
            usage->assignmentFromAst = true;
        }
        collectAssignmentTarget(lineUsages, *usage, lhs);
        // An onCleanup object is kept only to run its function when it is cleared.
        if (lhs->type == id_node && lhs->down == nullptr && isNamedParenCall(rhs, "onCleanup")) {
            appendUnique(usage->usageNotRequired, lhs->text);
        }
    }
    //=============================================================================
    void
    markMultiCallAssignment(
        std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->opNum != OP_MULTICALL || node->down == nullptr) {
            return;
        }
        AstSourceSpan span = firstSourceSpan(node->down);
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        if (!usage->assignmentFromAst) {
            usage->assigned.clear();
            usage->leftModified.clear();
            usage->assignmentFromAst = true;
        }
        // Only the bracket targets: the callee is the right sibling of the bracket node.
        collectAssignmentTarget(lineUsages, *usage, node->down->down);
    }
    //=============================================================================
    void
    markForLoopUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != reserved_node || node->tokenNumber != NLS_KEYWORD_FOR
            || node->down == nullptr || node->down->type != id_node) {
            return;
        }
        AbstractSyntaxTreePtr loopVariable = node->down;
        AstSourceSpan span = firstSourceSpan(loopVariable);
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        if (!usage->assignmentFromAst) {
            usage->assigned.clear();
            usage->leftModified.clear();
            usage->assignmentFromAst = true;
        }
        if (isTrackedIdentifier(loopVariable->text)) {
            appendUnique(usage->assigned, loopVariable->text);
            appendUnique(usage->usageNotRequired, loopVariable->text);
        }
        markExpressionNodeUsage(lineUsages, loopVariable->down);
    }
    //=============================================================================
    void
    markSwitchUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != reserved_node
            || node->tokenNumber != NLS_KEYWORD_SWITCH) {
            return;
        }
        markExpressionNodeUsage(lineUsages, node->down);
    }
    //=============================================================================
    void
    markCaseUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != reserved_node
            || node->tokenNumber != NLS_KEYWORD_CASE) {
            return;
        }
        markExpressionNodeUsage(lineUsages, node->down);
    }
    //=============================================================================
    void
    markCatchVariable(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != id_node || !isTrackedIdentifier(node->text)) {
            return;
        }
        AstSourceSpan span = spanFromContext(node->getContext());
        bool directCatchLine = false;
        if (span.line > 0 && span.line <= static_cast<int>(lines.size())) {
            const std::string code
                = codeWithoutStringsAndComments(lines[static_cast<size_t>(span.line - 1)]);
            directCatchLine = startsWithWord(toLowerAscii(trim(code)), "catch");
        }
        if (!directCatchLine) {
            span = firstSourceSpan(node->down);
            const int lastLine = std::min(span.line - 1, static_cast<int>(lines.size()));
            for (int line = lastLine; line > 0; --line) {
                const std::string code
                    = codeWithoutStringsAndComments(lines[static_cast<size_t>(line - 1)]);
                if (startsWithWord(toLowerAscii(trim(code)), "catch")
                    && codeAnalyzerWordColumnInLine(code, node->text) > 1) {
                    span.line = line;
                    break;
                }
            }
        }
        if (span.line <= 0 || span.line > static_cast<int>(lines.size())) {
            return;
        }
        const std::string code
            = codeWithoutStringsAndComments(lines[static_cast<size_t>(span.line - 1)]);
        if (!startsWithWord(toLowerAscii(trim(code)), "catch")) {
            return;
        }
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        if (!usage->assignmentFromAst) {
            usage->assigned.clear();
            usage->leftModified.clear();
            usage->assignmentFromAst = true;
        }
        appendUnique(usage->assigned, node->text);
        // The catch variable names the error, it does not need a later use.
        appendUnique(usage->usageNotRequired, node->text);
        usage->used.erase(
            std::remove(usage->used.begin(), usage->used.end(), node->text), usage->used.end());
    }
    //=============================================================================
    // save(file, 'name') reads the variable 'name'.
    // Variable names given as text to save, savenh5 or savemat.
    std::vector<AbstractSyntaxTreePtr>
    saveByNameArguments(AbstractSyntaxTreePtr node)
    {
        std::vector<AbstractSyntaxTreePtr> names;
        if (!isNamedParenCall(node, "save") && !isNamedParenCall(node, "savenh5")
            && !isNamedParenCall(node, "savemat")) {
            return names;
        }
        node = unwrapRhs(node);
        AbstractSyntaxTreePtr file = node->down->down;
        for (AbstractSyntaxTreePtr arg = file == nullptr ? nullptr : file->right; arg != nullptr;
            arg = arg->right) {
            AbstractSyntaxTreePtr name = unwrapRhs(arg);
            if (name != nullptr
                && (name->type == const_character_array_node || name->type == const_string_node)
                && isTrackedIdentifier(name->text) && name->text.front() != '-') {
                names.push_back(name);
            }
        }
        return names;
    }
    //=============================================================================
    void
    markSaveByNameCall(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        for (AbstractSyntaxTreePtr name : saveByNameArguments(node)) {
            CodeAnalyzerLineUsage* usage = usageAt(lineUsages, firstSourceSpan(name).line);
            if (usage != nullptr) {
                prepareAstUsage(*usage);
                appendUnique(usage->used, name->text);
            }
        }
    }
    //=============================================================================
    void
    markLiteralImportCall(
        std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != id_node || node->text != "import"
            || node->down == nullptr || node->down->opNum != OP_PARENS) {
            return;
        }
        AstSourceSpan span = spanFromContext(node->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        bool hasImport = false;
        for (AbstractSyntaxTreePtr arg = node->down->down; arg != nullptr; arg = arg->right) {
            AbstractSyntaxTreePtr specNode = unwrapRhs(arg);
            if (specNode == nullptr
                || (specNode->type != const_character_array_node
                    && specNode->type != const_string_node)) {
                continue;
            }
            appendImportedSpec(*usage, specNode->text);
            markImportLineRange(lineUsages, span.line, firstSourceSpan(specNode).line);
            hasImport = true;
        }
        usage->importLine = usage->importLine || hasImport;
        usage->importFromAst = usage->importFromAst || hasImport;
    }
    //=============================================================================
    void
    markCommandImportCall(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != id_node || node->text != "import"
            || node->down != nullptr || node->right == nullptr) {
            return;
        }
        AstSourceSpan span = spanFromContext(node->getContext());
        if (span.line <= 0 || span.line > static_cast<int>(lines.size())) {
            return;
        }
        const std::string code
            = codeWithoutStringsAndComments(lines[static_cast<size_t>(span.line - 1)]);
        if (!startsWithWord(toLowerAscii(trim(code)), "import")) {
            return;
        }
        const int endLine = logicalStatementEndLine(lines, span.line);
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        bool hasImport = false;
        for (AbstractSyntaxTreePtr spec = node->right; spec != nullptr; spec = spec->right) {
            AstSourceSpan specSpan = firstSourceSpan(spec);
            if (specSpan.line < span.line || specSpan.line > endLine) {
                break;
            }
            const std::string dottedName = staticDottedNameFromAst(spec);
            if (dottedName.empty()) {
                continue;
            }
            appendImportedSpec(*usage, dottedName);
            markImportLineRange(lineUsages, span.line, specSpan.line);
            hasImport = true;
        }
        usage->importLine = usage->importLine || hasImport;
        usage->importFromAst = usage->importFromAst || hasImport;
    }
    //=============================================================================
    void
    markDeclarationCall(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr) {
            return;
        }
        AbstractSyntaxTreePtr head = unwrapRhs(node);
        if (head != nullptr && head->type == non_terminal && head->opNum == OP_SCALL) {
            head = unwrapRhs(head->down);
        }
        if (head == nullptr || head->type != id_node
            || (head->text != "global" && head->text != "persistent")) {
            return;
        }
        AstSourceSpan span = spanFromContext(head->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        stringVector& declarations
            = head->text == "global" ? usage->globalDeclarations : usage->persistentDeclarations;
        auto appendDeclaration
            = [&](auto&& appendDeclarationRef, AbstractSyntaxTreePtr arg) -> void {
            if (arg == nullptr || arg == head) {
                return;
            }
            AstSourceSpan argSpan = firstSourceSpan(arg);
            if (argSpan.line > 0 && span.line > 0 && argSpan.line != span.line) {
                return;
            }
            if (arg->type == id_node && isTrackedIdentifier(arg->text) && arg->text != "global"
                && arg->text != "persistent") {
                appendUnique(declarations, arg->text);
            }
            appendDeclarationRef(appendDeclarationRef, arg->down);
            appendDeclarationRef(appendDeclarationRef, arg->right);
        };
        appendDeclaration(appendDeclaration, head->right);
        appendDeclaration(appendDeclaration, head->down);
    }
    //=============================================================================
    void
    markDynamicAstCall(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node, const std::string& name)
    {
        AstSourceSpan span = spanFromContext(node->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        usage->dynamicCode = true;
        usage->dynamicCallName = name;
        usage->dynamicCallColumn
            = codeAnalyzerWordColumnInLine(lines[static_cast<size_t>(span.line - 1)], name);
    }
    //=============================================================================
    std::string
    exitStatementKeyword(unsigned int tokenNumber)
    {
        if (tokenNumber == NLS_KEYWORD_BREAK) {
            return "break";
        }
        if (tokenNumber == NLS_KEYWORD_CONTINUE) {
            return "continue";
        }
        return "return";
    }
    //=============================================================================
    bool
    exitKeywordEndsStatementLine(const std::string& line, const std::string& keyword)
    {
        std::string code = CodeAnalyzerAst::trim(CodeAnalyzerAst::stripComment(line));
        while (!code.empty() && (code.back() == ';' || code.back() == ',')) {
            code.pop_back();
            code = CodeAnalyzerAst::trim(code);
        }
        if (code.size() < keyword.size()) {
            return false;
        }
        if (code.compare(code.size() - keyword.size(), keyword.size(), keyword) != 0) {
            return false;
        }
        if (code.size() == keyword.size()) {
            return true;
        }
        return !CodeAnalyzerAst::isIdentifierChar(code[code.size() - keyword.size() - 1]);
    }
    //=============================================================================
    // Only an exit keyword that terminates the statement text of its own line makes the
    // following line unconditionally unreachable. A compound one-liner such as
    // "if cond, return; end" leaves the rest of the block reachable.
    void
    markExitStatement(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node, const std::string& keyword)
    {
        AstSourceSpan span = spanFromContext(node->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        if (!exitKeywordEndsStatementLine(lines[static_cast<size_t>(span.line - 1)], keyword)) {
            return;
        }
        usage->exitStatement = true;
    }
    //=============================================================================
    // A switch is not listed: its first line is always a case.
    bool
    isFlowBlockStartKeyword(unsigned int tokenNumber)
    {
        return tokenNumber == NLS_KEYWORD_IF || tokenNumber == NLS_KEYWORD_FOR
            || tokenNumber == NLS_KEYWORD_WHILE || tokenNumber == NLS_KEYWORD_TRY;
    }
    //=============================================================================
    void
    markFlowBlockStart(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node)
    {
        AstSourceSpan span = firstSourceSpan(node);
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        // A block closed or holding a statement on its own line is not empty.
        const std::string& line = lines[static_cast<size_t>(span.line - 1)];
        usage->flowBlockStart = inlineBlockBalance(line) > 0 && !blockHeaderHasStatement(line);
    }
    //=============================================================================
    void
    markFloatComparison(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node)
    {
        AstSourceSpan span = spanFromContext(node->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        usage->suspiciousFloatComparison = true;
        usage->suspiciousFloatComparisonColumn
            = span.column > 0 ? span.column : operatorColumnInLine(lines, span.line, node->opNum);
    }
    //=============================================================================
    void
    markNestedSprintfCall(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node, bool displayCall)
    {
        AstSourceSpan span = spanFromContext(node->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        const int column
            = codeAnalyzerWordColumnInLine(lines[static_cast<size_t>(span.line - 1)], node->text);
        if (displayCall) {
            usage->displaySprintfCall = true;
            usage->displaySprintfCallColumn = column;
        } else {
            usage->diagnosticSprintfCall = true;
            usage->diagnosticSprintfCallColumn = column;
        }
    }
    //=============================================================================
    void
    applyAstLineUsageFromParserContext(const ParserContext& parserContext,
        const stringVector& lines, std::vector<CodeAnalyzerLineUsage>& lineUsages);
    //=============================================================================
    AbstractSyntaxTreePtr
    nthCallArgument(AbstractSyntaxTreePtr call, int index)
    {
        AbstractSyntaxTreePtr arg = unwrapRhs(call)->down->down;
        for (int k = 0; k < index && arg != nullptr; ++k) {
            arg = arg->right;
        }
        return unwrapRhs(arg);
    }
    //=============================================================================
    bool
    isTextLiteral(AbstractSyntaxTreePtr node)
    {
        return node != nullptr
            && (node->type == const_character_array_node || node->type == const_string_node);
    }
    //=============================================================================
    // Code given as text to eval, evalc or evalin reads and assigns names on this line.
    void
    markEvaluatedTextUsage(
        std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        AbstractSyntaxTreePtr code = nullptr;
        if (isNamedParenCall(node, "eval") || isNamedParenCall(node, "evalc")) {
            code = nthCallArgument(node, 0);
        } else if (isNamedParenCall(node, "evalin")) {
            code = nthCallArgument(node, 1);
        }
        if (!isTextLiteral(code) || code->text.empty()) {
            return;
        }
        CodeAnalyzerLineUsage* usage
            = usageAt(lineUsages, spanFromContext(unwrapRhs(node)->getContext()).line);
        if (usage == nullptr) {
            return;
        }
        LexerContext lexerContext;
        ParserContext context = parseStringResult(lexerContext, code->text);
        if (context.state() == ParseError) {
            return;
        }
        stringVector codeLines;
        std::istringstream stream(code->text);
        for (std::string codeLine; std::getline(stream, codeLine);) {
            codeLines.push_back(codeLine);
        }
        std::vector<CodeAnalyzerLineUsage> inner(codeLines.size());
        for (size_t k = 0; k < inner.size(); ++k) {
            inner[k].line = static_cast<int>(k + 1);
        }
        applyAstLineUsageFromParserContext(context, codeLines, inner);
        prepareAstUsage(*usage);
        if (!usage->assignmentFromAst) {
            usage->assigned.clear();
            usage->leftModified.clear();
            usage->assignmentFromAst = true;
        }
        for (const CodeAnalyzerLineUsage& innerUsage : inner) {
            for (const std::string& name : innerUsage.used) {
                appendUnique(usage->used, name);
            }
            for (const std::string& name : innerUsage.selfRead) {
                appendUnique(usage->used, name);
            }
            for (const std::string& name : innerUsage.callableUsed) {
                appendUnique(usage->callableUsed, name);
            }
            for (const std::string& name : innerUsage.assigned) {
                appendUnique(usage->assigned, name);
            }
        }
    }
    //=============================================================================
    // run, loadnh5 or loadmat without an output adds variables to the workspace.
    void
    markWorkspaceLoadCall(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node)
    {
        const bool run = isNamedParenCall(node, "run");
        if (!run && !isNamedParenCall(node, "loadnh5") && !isNamedParenCall(node, "loadmat")) {
            return;
        }
        AstSourceSpan span = spanFromContext(unwrapRhs(node)->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        const std::string& name = unwrapRhs(node)->text;
        const int column
            = codeAnalyzerWordColumnInLine(lines[static_cast<size_t>(span.line - 1)], name);
        if (run || !hasAssignmentBeforeColumn(lines, span.line, column)) {
            usage->workspaceChanged = true;
        }
    }
    //=============================================================================
    // exist('name', 'var') checks the variable before it is used.
    void
    markExistVariableCall(
        std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (!isNamedParenCall(node, "exist")) {
            return;
        }
        AbstractSyntaxTreePtr name = nthCallArgument(node, 0);
        AbstractSyntaxTreePtr kind = nthCallArgument(node, 1);
        if (!isTextLiteral(name) || !isTextLiteral(kind) || kind->text != "var") {
            return;
        }
        CodeAnalyzerLineUsage* usage
            = usageAt(lineUsages, spanFromContext(unwrapRhs(node)->getContext()).line);
        if (usage != nullptr) {
            appendUnique(usage->existChecked, name->text);
        }
    }
    //=============================================================================
    void
    markIsemptyStrfindCall(std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const stringVector& lines, AbstractSyntaxTreePtr node)
    {
        if (!isNamedParenCall(node, "isempty")) {
            return;
        }
        node = unwrapRhs(node);
        if (node->down->down == nullptr || node->down->down->right != nullptr
            || !isNamedParenCall(node->down->down, "strfind")) {
            return;
        }
        // A cell element or a field may not be text, so contains could behave differently.
        AbstractSyntaxTreePtr strfindCall = unwrapRhs(node->down->down);
        for (AbstractSyntaxTreePtr arg = strfindCall->down->down; arg != nullptr;
            arg = arg->right) {
            AbstractSyntaxTreePtr value = unwrapRhs(arg);
            if (value == nullptr || value->type != id_node) {
                continue;
            }
            for (AbstractSyntaxTreePtr postfix = value->down; postfix != nullptr;
                postfix = postfix->right) {
                if (postfix->opNum == OP_BRACES || postfix->opNum == OP_DOT
                    || postfix->opNum == OP_DOTDYN) {
                    return;
                }
            }
        }
        AstSourceSpan span = spanFromContext(node->getContext());
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr || usage->isemptyStrfindCall) {
            return;
        }
        const std::string& line = lines[static_cast<size_t>(span.line - 1)];
        int column = codeAnalyzerWordColumnInLine(line, "isempty");
        size_t before = static_cast<size_t>(column - 1);
        while (before > 0 && std::isspace(static_cast<unsigned char>(line[before - 1]))) {
            --before;
        }
        usage->isemptyStrfindCall = true;
        usage->isemptyStrfindNegated = before > 0 && line[before - 1] == '~';
        usage->isemptyStrfindColumn
            = usage->isemptyStrfindNegated ? static_cast<int>(before) : column;
    }
    //=============================================================================
    // Variables assigned so far, those holding an empty value, and the loop indexes.
    struct LoopGrowthState
    {
        std::set<std::string> definedVariables;
        std::set<std::string> emptyVariables;
        std::vector<std::string> loopVariables;
    };
    //=============================================================================
    bool
    isEmptyLiteral(AbstractSyntaxTreePtr node)
    {
        node = unwrapRhs(node);
        return node != nullptr
            && (node->opNum == OP_EMPTY || node->opNum == OP_EMPTY_CELL
                || ((node->type == const_character_array_node || node->type == const_string_node)
                    && node->text.empty()));
    }
    //=============================================================================
    // x(end + 1), or x(k) with k the loop index and x empty or undefined before the loop.
    // Only the first index counts: s.f(end + 1) or c{i}(end + 1) grows an element.
    bool
    indexedAssignmentGrows(AbstractSyntaxTreePtr lhs, const LoopGrowthState& state)
    {
        AbstractSyntaxTreePtr postfix = lhs->down;
        if (postfix->opNum != OP_PARENS && postfix->opNum != OP_BRACES) {
            return false;
        }
        const bool wasEmpty = state.emptyVariables.count(lhs->text) != 0
            || state.definedVariables.count(lhs->text) == 0;
        for (AbstractSyntaxTreePtr arg = postfix->down; arg != nullptr; arg = arg->right) {
            AbstractSyntaxTreePtr index = unwrapRhs(arg);
            if (index == nullptr) {
                continue;
            }
            if (index->opNum == OP_PLUS && index->down != nullptr
                && index->down->type == reserved_node
                && index->down->tokenNumber == NLS_KEYWORD_END) {
                return true;
            }
            if (wasEmpty && index->type == id_node && index->down == nullptr
                && std::find(state.loopVariables.begin(), state.loopVariables.end(), index->text)
                    != state.loopVariables.end()) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    // a:-1:b allocates the whole array on the first iteration.
    bool
    isDescendingRange(AbstractSyntaxTreePtr range)
    {
        range = unwrapRhs(range);
        return range != nullptr && range->opNum == OP_COLON && range->down != nullptr
            && range->down->opNum == OP_COLON && range->down->down != nullptr
            && range->down->down->right != nullptr && range->down->down->right->opNum == OP_UMINUS;
    }
    //=============================================================================
    // x = [x, value] or x = [value; x].
    bool
    concatenationGrows(const std::string& name, AbstractSyntaxTreePtr rhs)
    {
        rhs = unwrapRhs(rhs);
        if (rhs == nullptr || rhs->opNum != OP_BRACKETS) {
            return false;
        }
        bool found = false;
        int count = 0;
        for (AbstractSyntaxTreePtr row = rhs->down; row != nullptr; row = row->right) {
            for (AbstractSyntaxTreePtr element = row->down; element != nullptr;
                element = element->right) {
                AbstractSyntaxTreePtr value = unwrapRhs(element);
                found = found
                    || (value != nullptr && value->type == id_node && value->down == nullptr
                        && value->text == name);
                ++count;
            }
        }
        return found && count > 1;
    }
    //=============================================================================
    // error(id, message, ...), rethrow(e) and throw(e) leave the current code path.
    bool
    isThrowingCall(AbstractSyntaxTreePtr node)
    {
        if (isNamedParenCall(node, "error")) {
            return unwrapRhs(node)->down->down != nullptr
                && unwrapRhs(node)->down->down->right != nullptr;
        }
        return (isNamedParenCall(node, "rethrow") || isNamedParenCall(node, "throw"))
            && unwrapRhs(node)->down->down != nullptr;
    }
    //=============================================================================
    // The throwing call is the whole statement, and the statement ends on the returned line.
    int
    throwingStatementEndLine(const stringVector& lines, int line, const std::string& name)
    {
        if (line <= 0 || line > static_cast<int>(lines.size())) {
            return 0;
        }
        std::string code
            = trim(codeWithoutStringsAndComments(lines[static_cast<size_t>(line - 1)]));
        if (!startsWithWord(code, name)) {
            return 0;
        }
        while (code.size() >= 3 && code.compare(code.size() - 3, 3, "...") == 0
            && line < static_cast<int>(lines.size())) {
            ++line;
            code = trim(codeWithoutStringsAndComments(lines[static_cast<size_t>(line - 1)]));
        }
        if (!code.empty() && code.back() == ';') {
            code.pop_back();
        }
        return !code.empty() && code.back() == ')' ? line : 0;
    }
    //=============================================================================
    bool
    isConstantValue(AbstractSyntaxTreePtr condition, bool value)
    {
        if (!isConstantConditionLiteral(condition)) {
            return false;
        }
        const std::string& text = unwrapRhs(condition)->text;
        return value ? (text == "true" || text == "1") : (text == "false" || text == "0");
    }
    //=============================================================================
    // A break that leaves this loop, not one of its inner loops.
    bool
    hasLoopBreak(AbstractSyntaxTreePtr node)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_BREAK) {
                return true;
            }
            if (current->type == reserved_node
                && (current->tokenNumber == NLS_KEYWORD_FOR
                    || current->tokenNumber == NLS_KEYWORD_WHILE)) {
                continue;
            }
            if (current->down != nullptr && hasLoopBreak(current->down)) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    void
    markUnreachableAt(std::vector<CodeAnalyzerLineUsage>& lineUsages, int line)
    {
        if (CodeAnalyzerLineUsage* usage = usageAt(lineUsages, line)) {
            usage->unreachableStart = true;
        }
    }
    //=============================================================================
    // The last statement of a block returns or throws.
    bool
    blockEndsWithExit(AbstractSyntaxTreePtr block)
    {
        if (block == nullptr || block->opNum != OP_BLOCK || block->down == nullptr) {
            return false;
        }
        AbstractSyntaxTreePtr last = block->down;
        while (last->right != nullptr) {
            last = last->right;
        }
        AbstractSyntaxTreePtr statement = last->down != nullptr ? last->down : last;
        return (statement->type == reserved_node && statement->tokenNumber == NLS_KEYWORD_RETURN)
            || isThrowingCall(statement);
    }
    //=============================================================================
    // Branches of an if statement that can never run.
    void
    markUnreachableIfBranches(std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const stringVector& lines, AbstractSyntaxTreePtr ifNode)
    {
        bool alwaysTaken = false;
        // Returns true once the remaining branches are reported: one report covers them.
        auto visitCondition = [&](AbstractSyntaxTreePtr cstat) {
            AbstractSyntaxTreePtr condition = cstat->down;
            if (condition == nullptr) {
                return false;
            }
            if (alwaysTaken) {
                markUnreachableAt(lineUsages, firstSourceSpan(condition).line);
                return true;
            }
            if (isConstantValue(condition, false) && condition->right != nullptr) {
                markUnreachableAt(lineUsages, firstSourceSpan(condition->right).line);
            }
            alwaysTaken = isConstantValue(condition, true);
            return false;
        };
        for (AbstractSyntaxTreePtr branch = ifNode->down; branch != nullptr;
            branch = branch->right) {
            if (branch->opNum == OP_CSTAT) {
                if (visitCondition(branch)) {
                    return;
                }
            } else if (branch->opNum == OP_ELSEIFBLOCK) {
                for (AbstractSyntaxTreePtr cstat = branch->down; cstat != nullptr;
                    cstat = cstat->right) {
                    if (visitCondition(cstat)) {
                        return;
                    }
                }
            } else if (alwaysTaken) {
                // The block itself is located on 'else': report its first statement.
                AbstractSyntaxTreePtr first
                    = branch->down != nullptr && branch->down->down != nullptr ? branch->down->down
                                                                               : branch;
                markUnreachableAt(lineUsages, firstSourceSpan(first).line);
                return;
            }
        }
    }
    //=============================================================================
    void
    markUnreachableCode(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            if ((current->opNum == OP_RSTATEMENT || current->opNum == OP_QSTATEMENT)
                && current->down != nullptr) {
                AbstractSyntaxTreePtr statement = current->down;
                // while true without a break never reaches the next statement.
                if (statement->type == reserved_node && statement->tokenNumber == NLS_KEYWORD_WHILE
                    && statement->down != nullptr && isConstantValue(statement->down, true)
                    && !hasLoopBreak(statement->down->right) && current->right != nullptr) {
                    markUnreachableAt(lineUsages, firstSourceSpan(current->right).line);
                }
                // if true whose branch returns or throws never reaches the next statement.
                if (statement->type == reserved_node && statement->tokenNumber == NLS_KEYWORD_IF
                    && statement->down != nullptr && statement->down->opNum == OP_CSTAT
                    && statement->down->down != nullptr
                    && isConstantValue(statement->down->down, true)
                    && blockEndsWithExit(statement->down->down->right)
                    && current->right != nullptr) {
                    markUnreachableAt(lineUsages, firstSourceSpan(current->right).line);
                }
                if (isThrowingCall(statement)) {
                    const int line = throwingStatementEndLine(lines,
                        spanFromContext(unwrapRhs(statement)->getContext()).line,
                        unwrapRhs(statement)->text);
                    if (CodeAnalyzerLineUsage* usage = usageAt(lineUsages, line)) {
                        usage->exitStatement = true;
                    }
                }
            }
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_IF) {
                markUnreachableIfBranches(lineUsages, lines, current);
            }
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_WHILE
                && current->down != nullptr && isConstantValue(current->down, false)
                && current->down->right != nullptr) {
                markUnreachableAt(lineUsages, firstSourceSpan(current->down->right).line);
            }
            if (current->down != nullptr && current->opNum != OP_FUNCTION_HANDLE_ANONYMOUS) {
                markUnreachableCode(lineUsages, lines, current->down);
            }
        }
    }
    //=============================================================================
    // Target identifiers of 'x = ...' or '[a, b] = f(...)', indexed ones included.
    std::vector<AbstractSyntaxTreePtr>
    assignmentTargets(AbstractSyntaxTreePtr node)
    {
        std::vector<AbstractSyntaxTreePtr> targets;
        if (node == nullptr || node->down == nullptr) {
            return targets;
        }
        if (node->opNum == OP_ASSIGN) {
            if (node->down->type == id_node && isTrackedIdentifier(node->down->text)) {
                targets.push_back(node->down);
            }
            return targets;
        }
        if (node->opNum != OP_MULTICALL) {
            return targets;
        }
        for (AbstractSyntaxTreePtr row = node->down->down; row != nullptr; row = row->right) {
            for (AbstractSyntaxTreePtr element = row->down; element != nullptr;
                element = element->right) {
                AbstractSyntaxTreePtr target = unwrapRhs(element);
                if (target != nullptr && target->type == id_node
                    && isTrackedIdentifier(target->text)) {
                    targets.push_back(target);
                }
            }
        }
        return targets;
    }
    //=============================================================================
    // Inside a loop the right side may read the value of the previous iteration.
    void
    restoreLoopSelfReads(CodeAnalyzerLineUsage* usage)
    {
        if (usage == nullptr || usage->selfRead.empty()) {
            return;
        }
        for (const std::string& name : usage->selfRead) {
            appendUnique(usage->used, name);
        }
        usage->selfRead.clear();
    }
    //=============================================================================
    void
    markLoopGrowth(std::vector<CodeAnalyzerLineUsage>& lineUsages, const stringVector& lines,
        AbstractSyntaxTreePtr node, LoopGrowthState& state, int loopDepth)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_FOR
                && current->down != nullptr) {
                state.loopVariables.push_back(
                    isDescendingRange(current->down->down) ? std::string() : current->down->text);
                markLoopGrowth(lineUsages, lines, current->down->right, state, loopDepth + 1);
                state.loopVariables.pop_back();
                continue;
            }
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_WHILE
                && current->down != nullptr) {
                markLoopGrowth(lineUsages, lines, current->down->right, state, loopDepth + 1);
                continue;
            }
            if (current->opNum == OP_ASSIGN && current->down != nullptr
                && current->down->type == id_node) {
                AbstractSyntaxTreePtr lhs = current->down;
                const bool grows = loopDepth > 0 && isTrackedIdentifier(lhs->text)
                    && (lhs->down != nullptr ? indexedAssignmentGrows(lhs, state)
                                             : concatenationGrows(lhs->text, lhs->right));
                CodeAnalyzerLineUsage* usage = usageAt(lineUsages, firstSourceSpan(lhs).line);
                if (loopDepth > 0) {
                    restoreLoopSelfReads(usage);
                }
                if (grows && usage != nullptr && usage->loopGrowthName.empty()) {
                    usage->loopGrowthName = lhs->text;
                    usage->loopGrowthColumn = codeAnalyzerWordColumnInLine(
                        lines[static_cast<size_t>(usage->line - 1)], lhs->text);
                }
                // Inside a loop the assignment may be conditional, so it keeps the empty state.
                if (lhs->down == nullptr && isEmptyLiteral(lhs->right)) {
                    state.emptyVariables.insert(lhs->text);
                } else if (lhs->down == nullptr && loopDepth == 0) {
                    state.emptyVariables.erase(lhs->text);
                }
                state.definedVariables.insert(lhs->text);
                continue;
            }
            if (current->opNum == OP_MULTICALL && current->down != nullptr) {
                if (loopDepth > 0) {
                    for (AbstractSyntaxTreePtr target : assignmentTargets(current)) {
                        restoreLoopSelfReads(usageAt(lineUsages, firstSourceSpan(target).line));
                    }
                }
                std::vector<CodeAnalyzerLineUsage> unusedLineUsages;
                CodeAnalyzerLineUsage targets;
                collectAssignmentTarget(unusedLineUsages, targets, current->down->down);
                state.definedVariables.insert(targets.assigned.begin(), targets.assigned.end());
                continue;
            }
            if (current->down != nullptr && current->opNum != OP_FUNCTION_HANDLE_ANONYMOUS) {
                markLoopGrowth(lineUsages, lines, current->down, state, loopDepth);
            }
        }
    }
    //=============================================================================
    // First bare (not called with parentheses) read of name in an expression.
    AbstractSyntaxTreePtr
    findBareRead(AbstractSyntaxTreePtr node, const std::string& name)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            if (current->opNum == OP_FUNCTION_HANDLE_ANONYMOUS
                || current->opNum == OP_FUNCTION_HANDLE_NAMED) {
                continue;
            }
            if (current->type == id_node && current->text == name
                && (current->down == nullptr || current->down->opNum != OP_PARENS)) {
                return current;
            }
            if (AbstractSyntaxTreePtr found = findBareRead(current->down, name)) {
                return found;
            }
        }
        return nullptr;
    }
    //=============================================================================
    using SelfReadCandidates
        = std::vector<std::pair<CodeAnalyzerLineUsage*, AbstractSyntaxTreePtr>>;
    //=============================================================================
    // Plain targets not yet read or assigned on their line, taken before the right side.
    SelfReadCandidates
    selfReadCandidates(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        SelfReadCandidates candidates;
        for (AbstractSyntaxTreePtr target : assignmentTargets(node)) {
            CodeAnalyzerLineUsage* usage = usageAt(lineUsages, firstSourceSpan(target).line);
            if (target->down != nullptr || usage == nullptr) {
                continue;
            }
            const auto has = [&](const stringVector& names) {
                return std::find(names.begin(), names.end(), target->text) != names.end();
            };
            if ((usage->usageFromAst && (has(usage->used) || has(usage->selfRead)))
                || (usage->assignmentFromAst && has(usage->assigned))) {
                continue;
            }
            candidates.emplace_back(usage, target);
        }
        return candidates;
    }
    //=============================================================================
    // Moves a bare read of a target in its own right side out of the plain uses.
    void
    markSelfReads(const SelfReadCandidates& candidates, AbstractSyntaxTreePtr rhs)
    {
        for (const auto& candidate : candidates) {
            CodeAnalyzerLineUsage* usage = candidate.first;
            const std::string& name = candidate.second->text;
            auto used = std::find(usage->used.begin(), usage->used.end(), name);
            AbstractSyntaxTreePtr read = findBareRead(rhs, name);
            if (used == usage->used.end() || read == nullptr
                || firstSourceSpan(read).line != firstSourceSpan(candidate.second).line) {
                continue;
            }
            usage->used.erase(used);
            usage->selfRead.push_back(name);
        }
    }
    //=============================================================================
    void
    applyAstLineUsage(AbstractSyntaxTreePtr node, std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const stringVector& lines)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            // Its usage comes from the enclosing expression, without its parameters.
            if (current->opNum == OP_FUNCTION_HANDLE_ANONYMOUS) {
                continue;
            }
            // The right side is read before its targets are assigned.
            if (current->opNum == OP_ASSIGN || current->opNum == OP_MULTICALL) {
                const SelfReadCandidates candidates = selfReadCandidates(lineUsages, current);
                markExpressionUsage(lineUsages, rightOperand(current));
                applyAstLineUsage(rightOperand(current), lineUsages, lines);
                markSelfReads(candidates, rightOperand(current));
                if (current->opNum == OP_ASSIGN) {
                    markAssignment(lineUsages, current);
                } else {
                    markMultiCallAssignment(lineUsages, current);
                }
                continue;
            }
            if (current->opNum == OP_RHS) {
                markExpressionUsage(lineUsages, current->down);
            }
            if (current->type == id_node && current->down != nullptr
                && current->down->opNum == OP_PARENS
                && (current->text == "eval" || current->text == "evalin"
                    || current->text == "assignin")) {
                markDynamicAstCall(lineUsages, lines, current, current->text);
            }
            if (isUnsafeLoadAstCall(lines, current)) {
                markDynamicAstCall(lineUsages, lines, current, "load");
            }
            if (isUnsafeLoadCommandAstCall(lines, current)) {
                markDynamicAstCall(lineUsages, lines, current, "load");
            }
            markLiteralImportCall(lineUsages, current);
            markSaveByNameCall(lineUsages, current);
            markIsemptyStrfindCall(lineUsages, lines, current);
            markEvaluatedTextUsage(lineUsages, current);
            markWorkspaceLoadCall(lineUsages, lines, current);
            markExistVariableCall(lineUsages, current);
            markCommandImportCall(lineUsages, lines, current);
            markDeclarationCall(lineUsages, current);
            if ((isNamedParenCall(current, "disp") || isNamedParenCall(current, "display"))
                && firstArgumentIsSprintf(current)) {
                markNestedSprintfCall(lineUsages, lines, current, true);
            }
            if ((isNamedParenCall(current, "error") || isNamedParenCall(current, "warning"))
                && firstArgumentIsSprintf(current)) {
                markNestedSprintfCall(lineUsages, lines, current, false);
            }
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_IF) {
                markExpressionNodeUsage(
                    lineUsages, current->down == nullptr ? nullptr : current->down->down);
                markIfConstantConditions(lineUsages, lines, current->down);
            }
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_WHILE) {
                markExpressionNodeUsage(lineUsages, current->down);
                markConstantCondition(lineUsages, lines, current->down);
            }
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_FOR) {
                markForLoopUsage(lineUsages, current);
            }
            markSwitchUsage(lineUsages, current);
            markCaseUsage(lineUsages, current);
            markCatchVariable(lineUsages, lines, current);
            if (current->type == reserved_node && isFlowBlockStartKeyword(current->tokenNumber)) {
                markFlowBlockStart(lineUsages, lines, current);
            }
            if (current->type == reserved_node
                && (current->tokenNumber == NLS_KEYWORD_RETURN
                    || current->tokenNumber == NLS_KEYWORD_BREAK
                    || current->tokenNumber == NLS_KEYWORD_CONTINUE)) {
                markExitStatement(
                    lineUsages, lines, current, exitStatementKeyword(current->tokenNumber));
            }
            if ((current->opNum == OP_EQ || current->opNum == OP_NEQ)
                && subtreeHasFloatingLiteral(current->down)) {
                markFloatComparison(lineUsages, lines, current);
            }
            if (current->down != nullptr) {
                applyAstLineUsage(current->down, lineUsages, lines);
            }
        }
    }
    //=============================================================================
    using NameSet = std::set<std::string>;
    //=============================================================================
    struct LivenessContext
    {
        std::vector<CodeAnalyzerLineUsage>* lineUsages = nullptr;
        NameSet exitLive;
        // Live after the innermost loop (break) and at its head (continue).
        std::vector<std::pair<NameSet, NameSet>> loops;
        // Live on entry of the enclosing catch blocks: any statement of a try may fail.
        NameSet catchLive;
        bool report = true;
        // When set, receives identifiers read without parentheses.
        std::vector<AbstractSyntaxTreePtr>* bareReads = nullptr;
        // Set when an evaluated text call is read: its line may assign variables.
        bool evaluatedText = false;
    };
    //=============================================================================
    void
    collectReadNode(
        AbstractSyntaxTreePtr node, NameSet& names, const NameSet& hidden, LivenessContext& ctx);
    //=============================================================================
    void
    collectReadList(
        AbstractSyntaxTreePtr node, NameSet& names, const NameSet& hidden, LivenessContext& ctx)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            collectReadNode(current, names, hidden, ctx);
        }
    }
    //=============================================================================
    // Names one expression may read, without its right peers.
    void
    collectReadNode(
        AbstractSyntaxTreePtr node, NameSet& names, const NameSet& hidden, LivenessContext& ctx)
    {
        if (node == nullptr || node->opNum == OP_FUNCTION_HANDLE_NAMED
            || (node->type == non_terminal && node->opNum == OP_DOT)) {
            return;
        }
        if (node->opNum == OP_FUNCTION_HANDLE_ANONYMOUS) {
            NameSet inner = hidden;
            AbstractSyntaxTreePtr body = node->down;
            if (body != nullptr && body->right != nullptr) {
                inner.insert(body->text);
                for (AbstractSyntaxTreePtr arg = body->down; arg != nullptr; arg = arg->right) {
                    inner.insert(arg->text);
                }
                body = body->right;
            }
            collectReadList(body, names, inner, ctx);
            return;
        }
        if (node->type != id_node) {
            collectReadList(node->down, names, hidden, ctx);
            return;
        }
        if (isTrackedIdentifier(node->text) && hidden.find(node->text) == hidden.end()) {
            names.insert(node->text);
            if (ctx.bareReads != nullptr
                && (node->down == nullptr || node->down->opNum != OP_PARENS)) {
                ctx.bareReads->push_back(node);
            }
        }
        for (AbstractSyntaxTreePtr postfix = node->down; postfix != nullptr;
            postfix = postfix->right) {
            if (postfix->opNum == OP_PARENS || postfix->opNum == OP_BRACES
                || postfix->opNum == OP_DOTDYN) {
                collectReadList(postfix->down, names, hidden, ctx);
            }
        }
        for (AbstractSyntaxTreePtr name : saveByNameArguments(node)) {
            names.insert(name->text);
        }
        // Evaluated text reads what its line reads.
        if (isNamedParenCall(node, "eval") || isNamedParenCall(node, "evalc")
            || isNamedParenCall(node, "evalin")) {
            ctx.evaluatedText = true;
            if (CodeAnalyzerLineUsage* usage
                = usageAt(*ctx.lineUsages, firstSourceSpan(node).line)) {
                names.insert(usage->used.begin(), usage->used.end());
                names.insert(usage->callableUsed.begin(), usage->callableUsed.end());
                names.insert(usage->selfRead.begin(), usage->selfRead.end());
            }
        }
    }
    //=============================================================================
    NameSet
    readNames(AbstractSyntaxTreePtr node, LivenessContext& ctx)
    {
        NameSet names;
        collectReadNode(node, names, {}, ctx);
        return names;
    }
    //=============================================================================
    void
    unionInto(NameSet& target, const NameSet& source)
    {
        target.insert(source.begin(), source.end());
    }
    //=============================================================================
    // A field assignment may update a handle object, so it is never reported.
    bool
    hasFieldReference(AbstractSyntaxTreePtr target)
    {
        for (AbstractSyntaxTreePtr postfix = target->down; postfix != nullptr;
            postfix = postfix->right) {
            if (postfix->opNum == OP_DOT || postfix->opNum == OP_DOTDYN) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    void
    recordDeadAssignment(LivenessContext& ctx, AbstractSyntaxTreePtr target)
    {
        AstSourceSpan span = firstSourceSpan(target);
        CodeAnalyzerLineUsage* usage = usageAt(*ctx.lineUsages, span.line);
        if (usage == nullptr
            || std::find(usage->deadAssignments.begin(), usage->deadAssignments.end(), target->text)
                != usage->deadAssignments.end()) {
            return;
        }
        // The parser column points just after the identifier.
        const int column = span.column - static_cast<int>(target->text.size());
        usage->deadAssignments.push_back(target->text);
        usage->deadAssignmentColumns.push_back(column > 0 ? column : 1);
    }
    //=============================================================================
    NameSet
    liveBeforeBlock(AbstractSyntaxTreePtr block, NameSet live, LivenessContext& ctx);
    //=============================================================================
    // Live names on loop entry: 'next' gives the head set from the body entry set.
    template <typename Next>
    NameSet
    loopHeadLive(AbstractSyntaxTreePtr body, const NameSet& after, NameSet head,
        LivenessContext& ctx, Next next)
    {
        const bool report = ctx.report;
        ctx.report = false;
        while (true) {
            ctx.loops.emplace_back(after, head);
            NameSet bodyIn = body == nullptr ? head : liveBeforeBlock(body, head, ctx);
            ctx.loops.pop_back();
            NameSet updated = next(bodyIn);
            if (updated == head) {
                break;
            }
            head = std::move(updated);
        }
        ctx.report = report;
        if (report && body != nullptr) {
            ctx.loops.emplace_back(after, head);
            liveBeforeBlock(body, head, ctx);
            ctx.loops.pop_back();
        }
        return head;
    }
    //=============================================================================
    NameSet
    liveBeforeIf(AbstractSyntaxTreePtr node, const NameSet& after, LivenessContext& ctx)
    {
        NameSet result;
        bool hasElse = false;
        auto visitCondition = [&](AbstractSyntaxTreePtr cstat) {
            AbstractSyntaxTreePtr condition = cstat->down;
            AbstractSyntaxTreePtr block = condition == nullptr ? nullptr : condition->right;
            unionInto(result, block == nullptr ? after : liveBeforeBlock(block, after, ctx));
            unionInto(result, readNames(condition, ctx));
        };
        for (AbstractSyntaxTreePtr branch = node->down; branch != nullptr; branch = branch->right) {
            if (branch->opNum == OP_CSTAT) {
                visitCondition(branch);
            } else if (branch->opNum == OP_ELSEIFBLOCK) {
                for (AbstractSyntaxTreePtr cstat = branch->down; cstat != nullptr;
                    cstat = cstat->right) {
                    visitCondition(cstat);
                }
            } else if (branch->opNum == OP_BLOCK) {
                hasElse = true;
                unionInto(result, liveBeforeBlock(branch, after, ctx));
            }
        }
        if (!hasElse) {
            unionInto(result, after);
        }
        return result;
    }
    //=============================================================================
    NameSet
    liveBeforeSwitch(AbstractSyntaxTreePtr node, const NameSet& after, LivenessContext& ctx)
    {
        AbstractSyntaxTreePtr selector = node->down;
        NameSet result = readNames(selector, ctx);
        bool hasOtherwise = false;
        for (AbstractSyntaxTreePtr part = selector == nullptr ? nullptr : selector->right;
            part != nullptr; part = part->right) {
            if (part->opNum == OP_CASEBLOCK) {
                for (AbstractSyntaxTreePtr item = part->down; item != nullptr; item = item->right) {
                    AbstractSyntaxTreePtr value = item->down;
                    AbstractSyntaxTreePtr block = value == nullptr ? nullptr : value->right;
                    unionInto(result, readNames(value, ctx));
                    unionInto(
                        result, block == nullptr ? after : liveBeforeBlock(block, after, ctx));
                }
            } else if (part->opNum == OP_BLOCK) {
                hasOtherwise = true;
                unionInto(result, liveBeforeBlock(part, after, ctx));
            }
        }
        if (!hasOtherwise) {
            unionInto(result, after);
        }
        return result;
    }
    //=============================================================================
    NameSet
    liveBeforeTry(AbstractSyntaxTreePtr node, const NameSet& after, LivenessContext& ctx)
    {
        AbstractSyntaxTreePtr body = node->down;
        AbstractSyntaxTreePtr handler = body == nullptr ? nullptr : body->right;
        // Without catch, an error goes on after the try block.
        NameSet catchIn = after;
        if (handler != nullptr && handler->type == id_node && handler->down != nullptr
            && handler->down->opNum == OP_BLOCK) {
            catchIn = liveBeforeBlock(handler->down, after, ctx);
            catchIn.erase(handler->text);
        } else if (handler != nullptr && handler->opNum == OP_BLOCK) {
            catchIn = liveBeforeBlock(handler, after, ctx);
        } else if (handler != nullptr) {
            unionInto(catchIn, readNames(handler, ctx));
        }
        NameSet saved = ctx.catchLive;
        unionInto(ctx.catchLive, catchIn);
        NameSet result = body == nullptr ? after : liveBeforeBlock(body, after, ctx);
        ctx.catchLive = std::move(saved);
        unionInto(result, catchIn);
        return result;
    }
    //=============================================================================
    void
    liveBeforeAssignment(AbstractSyntaxTreePtr node, NameSet& live, LivenessContext& ctx)
    {
        NameSet reads;
        NameSet defined;
        for (AbstractSyntaxTreePtr target : assignmentTargets(node)) {
            if (ctx.report && live.find(target->text) == live.end() && !hasFieldReference(target)) {
                recordDeadAssignment(ctx, target);
            }
            if (target->down == nullptr) {
                defined.insert(target->text);
            } else {
                // An indexed assignment keeps the other elements.
                reads.insert(target->text);
                for (AbstractSyntaxTreePtr postfix = target->down; postfix != nullptr;
                    postfix = postfix->right) {
                    if (postfix->opNum == OP_PARENS || postfix->opNum == OP_BRACES
                        || postfix->opNum == OP_DOTDYN) {
                        collectReadList(postfix->down, reads, {}, ctx);
                    }
                }
            }
        }
        for (const std::string& name : defined) {
            live.erase(name);
        }
        if (node->opNum == OP_ASSIGN) {
            collectReadList(rightOperand(node), reads, {}, ctx);
        } else {
            collectReadNode(rightOperand(node), reads, {}, ctx);
        }
        unionInto(live, reads);
    }
    //=============================================================================
    void
    liveBeforeStatement(AbstractSyntaxTreePtr node, NameSet& live, LivenessContext& ctx)
    {
        if (node == nullptr) {
            return;
        }
        if (node->opNum == OP_ASSIGN || node->opNum == OP_MULTICALL) {
            liveBeforeAssignment(node, live, ctx);
            return;
        }
        if (node->type == non_terminal && node->opNum == OP_SCALL) {
            // Command syntax words may name variables, as in 'save file x'.
            for (AbstractSyntaxTreePtr word = node->down; word != nullptr; word = word->right) {
                if (isTrackedIdentifier(word->text)) {
                    live.insert(word->text);
                }
            }
            return;
        }
        if (node->type != reserved_node) {
            unionInto(live, readNames(node, ctx));
            return;
        }
        switch (node->tokenNumber) {
        case NLS_KEYWORD_FOR: {
            AbstractSyntaxTreePtr index = node->down;
            AbstractSyntaxTreePtr body = index == nullptr ? nullptr : index->right;
            const std::string variable = index == nullptr ? std::string() : index->text;
            const NameSet after = live;
            live = loopHeadLive(body, after, after, ctx, [&](const NameSet& bodyIn) {
                NameSet head = after;
                for (const std::string& name : bodyIn) {
                    if (name != variable) {
                        head.insert(name);
                    }
                }
                return head;
            });
            if (index != nullptr) {
                collectReadList(index->down, live, {}, ctx);
            }
        } break;
        case NLS_KEYWORD_WHILE: {
            AbstractSyntaxTreePtr condition = node->down;
            AbstractSyntaxTreePtr body = condition == nullptr ? nullptr : condition->right;
            // The condition is read before each iteration and before leaving the loop.
            NameSet exitHead = live;
            unionInto(exitHead, readNames(condition, ctx));
            live = loopHeadLive(body, live, exitHead, ctx, [&](const NameSet& bodyIn) {
                NameSet head = exitHead;
                unionInto(head, bodyIn);
                return head;
            });
        } break;
        case NLS_KEYWORD_IF: {
            live = liveBeforeIf(node, live, ctx);
        } break;
        case NLS_KEYWORD_SWITCH: {
            live = liveBeforeSwitch(node, live, ctx);
        } break;
        case NLS_KEYWORD_TRY: {
            live = liveBeforeTry(node, live, ctx);
        } break;
        case NLS_KEYWORD_BREAK: {
            if (!ctx.loops.empty()) {
                live = ctx.loops.back().first;
            }
        } break;
        case NLS_KEYWORD_CONTINUE: {
            if (!ctx.loops.empty()) {
                live = ctx.loops.back().second;
            }
        } break;
        case NLS_KEYWORD_RETURN: {
            live = ctx.exitLive;
        } break;
        default: {
            collectReadList(node->down, live, {}, ctx);
        } break;
        }
    }
    //=============================================================================
    NameSet
    liveBeforeBlock(AbstractSyntaxTreePtr block, NameSet live, LivenessContext& ctx)
    {
        std::vector<AbstractSyntaxTreePtr> statements;
        AbstractSyntaxTreePtr first
            = block != nullptr && block->opNum == OP_BLOCK ? block->down : block;
        for (AbstractSyntaxTreePtr current = first; current != nullptr; current = current->right) {
            statements.push_back(current);
        }
        for (auto it = statements.rbegin(); it != statements.rend(); ++it) {
            AbstractSyntaxTreePtr statement = *it;
            if (statement->opNum == OP_RSTATEMENT || statement->opNum == OP_QSTATEMENT) {
                statement = statement->down;
            }
            liveBeforeStatement(statement, live, ctx);
            // A statement that fails assigns nothing and goes to catch.
            unionInto(live, ctx.catchLive);
        }
        return live;
    }
    //=============================================================================
    // Marks assignments whose value no later statement of the function can read.
    void
    markDeadAssignments(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr code,
        const stringVector& outputs)
    {
        LivenessContext ctx;
        ctx.lineUsages = &lineUsages;
        ctx.exitLive.insert(outputs.begin(), outputs.end());
        liveBeforeBlock(code, ctx.exitLive, ctx);
    }
    //=============================================================================
    // Names that may hold a value at one point; unreachable code has no state.
    struct DefinedState
    {
        bool reachable = true;
        NameSet names;
        bool
        operator==(const DefinedState& other) const
        {
            return reachable == other.reachable && names == other.names;
        }
    };
    //=============================================================================
    void
    mergeState(DefinedState& target, const DefinedState& source)
    {
        if (!source.reachable) {
            return;
        }
        if (!target.reachable) {
            target = source;
            return;
        }
        unionInto(target.names, source.names);
    }
    //=============================================================================
    DefinedState
    unreachableState()
    {
        DefinedState state;
        state.reachable = false;
        return state;
    }
    //=============================================================================
    struct DefinednessContext
    {
        std::vector<CodeAnalyzerLineUsage>* lineUsages = nullptr;
        // States leaving the innermost loop (break) and going back to its head (continue).
        std::vector<std::pair<DefinedState, DefinedState>> loops;
        // States seen in the enclosing try blocks: an error may jump to their catch.
        std::vector<DefinedState*> tries;
        bool report = true;
    };
    //=============================================================================
    void
    recordUndefinedRead(DefinednessContext& ctx, AbstractSyntaxTreePtr read)
    {
        AstSourceSpan span = firstSourceSpan(read);
        CodeAnalyzerLineUsage* usage = usageAt(*ctx.lineUsages, span.line);
        if (usage == nullptr
            || std::find(usage->undefinedReads.begin(), usage->undefinedReads.end(), read->text)
                != usage->undefinedReads.end()) {
            return;
        }
        // The parser column points just after the identifier.
        const int column = span.column - static_cast<int>(read->text.size());
        usage->undefinedReads.push_back(read->text);
        usage->undefinedReadColumns.push_back(column > 0 ? column : 1);
    }
    //=============================================================================
    // Checks the reads of an expression (and its right peers when 'list' is set).
    void
    checkReads(AbstractSyntaxTreePtr node, bool list, DefinedState& state, DefinednessContext& ctx)
    {
        std::vector<AbstractSyntaxTreePtr> reads;
        LivenessContext reader;
        reader.lineUsages = ctx.lineUsages;
        reader.bareReads = &reads;
        NameSet names;
        if (list) {
            collectReadList(node, names, {}, reader);
        } else {
            collectReadNode(node, names, {}, reader);
        }
        if (ctx.report && state.reachable) {
            for (AbstractSyntaxTreePtr read : reads) {
                if (state.names.find(read->text) == state.names.end()) {
                    recordUndefinedRead(ctx, read);
                }
            }
        }
        // Evaluated text may assign the variables its line assigns.
        if (reader.evaluatedText && node != nullptr) {
            if (CodeAnalyzerLineUsage* usage
                = usageAt(*ctx.lineUsages, firstSourceSpan(node).line)) {
                state.names.insert(usage->assigned.begin(), usage->assigned.end());
            }
        }
    }
    //=============================================================================
    DefinedState
    definedAfterBlock(AbstractSyntaxTreePtr block, DefinedState state, DefinednessContext& ctx);
    //=============================================================================
    // Runs a loop body to a fixed point; 'enter' gives the body state from the head state.
    template <typename Enter>
    DefinedState
    definedAfterLoop(AbstractSyntaxTreePtr condition, AbstractSyntaxTreePtr body,
        const DefinedState& entry, DefinednessContext& ctx, Enter enter)
    {
        auto runBody = [&](const DefinedState& head, std::pair<DefinedState, DefinedState>& exits) {
            DefinedState in = head;
            checkReads(condition, false, in, ctx);
            enter(in);
            ctx.loops.emplace_back(unreachableState(), unreachableState());
            DefinedState out = body == nullptr ? in : definedAfterBlock(body, in, ctx);
            exits = ctx.loops.back();
            ctx.loops.pop_back();
            return out;
        };
        const bool report = ctx.report;
        ctx.report = false;
        DefinedState head = entry;
        std::pair<DefinedState, DefinedState> exits;
        while (true) {
            DefinedState next = entry;
            mergeState(next, runBody(head, exits));
            mergeState(next, exits.second);
            if (next == head) {
                break;
            }
            head = std::move(next);
        }
        ctx.report = report;
        runBody(head, exits);
        DefinedState after = head;
        mergeState(after, exits.first);
        return after;
    }
    //=============================================================================
    void
    definedAfterStatement(AbstractSyntaxTreePtr node, DefinedState& state, DefinednessContext& ctx)
    {
        if (node == nullptr) {
            return;
        }
        if (node->opNum == OP_ASSIGN || node->opNum == OP_MULTICALL) {
            const std::vector<AbstractSyntaxTreePtr> targets = assignmentTargets(node);
            for (AbstractSyntaxTreePtr target : targets) {
                for (AbstractSyntaxTreePtr postfix = target->down; postfix != nullptr;
                    postfix = postfix->right) {
                    if (postfix->opNum == OP_PARENS || postfix->opNum == OP_BRACES
                        || postfix->opNum == OP_DOTDYN) {
                        checkReads(postfix->down, true, state, ctx);
                    }
                }
            }
            checkReads(rightOperand(node), node->opNum == OP_ASSIGN, state, ctx);
            for (AbstractSyntaxTreePtr target : targets) {
                state.names.insert(target->text);
            }
            return;
        }
        // Command syntax words are text.
        if (node->type == non_terminal && node->opNum == OP_SCALL) {
            return;
        }
        if (node->type != reserved_node) {
            checkReads(node, false, state, ctx);
            return;
        }
        switch (node->tokenNumber) {
        case NLS_KEYWORD_FOR: {
            AbstractSyntaxTreePtr index = node->down;
            if (index == nullptr) {
                break;
            }
            checkReads(index->down, true, state, ctx);
            const std::string variable = index->text;
            state = definedAfterLoop(nullptr, index->right, state, ctx,
                [&](DefinedState& in) { in.names.insert(variable); });
        } break;
        case NLS_KEYWORD_WHILE: {
            AbstractSyntaxTreePtr condition = node->down;
            state = definedAfterLoop(condition, condition == nullptr ? nullptr : condition->right,
                state, ctx, [](DefinedState&) { });
        } break;
        case NLS_KEYWORD_IF: {
            DefinedState after = unreachableState();
            bool hasElse = false;
            auto visitCondition = [&](AbstractSyntaxTreePtr cstat) {
                AbstractSyntaxTreePtr condition = cstat->down;
                checkReads(condition, false, state, ctx);
                AbstractSyntaxTreePtr block = condition == nullptr ? nullptr : condition->right;
                mergeState(after, block == nullptr ? state : definedAfterBlock(block, state, ctx));
            };
            for (AbstractSyntaxTreePtr branch = node->down; branch != nullptr;
                branch = branch->right) {
                if (branch->opNum == OP_CSTAT) {
                    visitCondition(branch);
                } else if (branch->opNum == OP_ELSEIFBLOCK) {
                    for (AbstractSyntaxTreePtr cstat = branch->down; cstat != nullptr;
                        cstat = cstat->right) {
                        visitCondition(cstat);
                    }
                } else if (branch->opNum == OP_BLOCK) {
                    hasElse = true;
                    mergeState(after, definedAfterBlock(branch, state, ctx));
                }
            }
            if (!hasElse) {
                mergeState(after, state);
            }
            state = std::move(after);
        } break;
        case NLS_KEYWORD_SWITCH: {
            AbstractSyntaxTreePtr selector = node->down;
            checkReads(selector, false, state, ctx);
            DefinedState after = unreachableState();
            bool hasOtherwise = false;
            for (AbstractSyntaxTreePtr part = selector == nullptr ? nullptr : selector->right;
                part != nullptr; part = part->right) {
                if (part->opNum == OP_CASEBLOCK) {
                    for (AbstractSyntaxTreePtr item = part->down; item != nullptr;
                        item = item->right) {
                        AbstractSyntaxTreePtr value = item->down;
                        checkReads(value, false, state, ctx);
                        AbstractSyntaxTreePtr block = value == nullptr ? nullptr : value->right;
                        mergeState(
                            after, block == nullptr ? state : definedAfterBlock(block, state, ctx));
                    }
                } else if (part->opNum == OP_BLOCK) {
                    hasOtherwise = true;
                    mergeState(after, definedAfterBlock(part, state, ctx));
                }
            }
            if (!hasOtherwise) {
                mergeState(after, state);
            }
            state = std::move(after);
        } break;
        case NLS_KEYWORD_TRY: {
            AbstractSyntaxTreePtr body = node->down;
            AbstractSyntaxTreePtr handler = body == nullptr ? nullptr : body->right;
            DefinedState thrown = state;
            ctx.tries.push_back(&thrown);
            DefinedState after = body == nullptr ? state : definedAfterBlock(body, state, ctx);
            ctx.tries.pop_back();
            if (handler != nullptr && handler->type == id_node && handler->down != nullptr
                && handler->down->opNum == OP_BLOCK) {
                if (thrown.reachable) {
                    thrown.names.insert(handler->text);
                }
                mergeState(after, definedAfterBlock(handler->down, thrown, ctx));
            } else if (handler != nullptr && handler->opNum == OP_BLOCK) {
                mergeState(after, definedAfterBlock(handler, thrown, ctx));
            } else {
                mergeState(after, thrown);
            }
            state = std::move(after);
        } break;
        case NLS_KEYWORD_BREAK:
        case NLS_KEYWORD_CONTINUE: {
            if (!ctx.loops.empty()) {
                mergeState(node->tokenNumber == NLS_KEYWORD_BREAK ? ctx.loops.back().first
                                                                  : ctx.loops.back().second,
                    state);
                state = unreachableState();
            }
        } break;
        case NLS_KEYWORD_RETURN: {
            state = unreachableState();
        } break;
        default: {
            checkReads(node->down, true, state, ctx);
        } break;
        }
    }
    //=============================================================================
    DefinedState
    definedAfterBlock(AbstractSyntaxTreePtr block, DefinedState state, DefinednessContext& ctx)
    {
        AbstractSyntaxTreePtr first
            = block != nullptr && block->opNum == OP_BLOCK ? block->down : block;
        for (AbstractSyntaxTreePtr current = first; current != nullptr; current = current->right) {
            AbstractSyntaxTreePtr statement = current;
            if (statement->opNum == OP_RSTATEMENT || statement->opNum == OP_QSTATEMENT) {
                statement = statement->down;
            }
            // A statement that fails assigns nothing and goes to catch.
            for (DefinedState* thrown : ctx.tries) {
                mergeState(*thrown, state);
            }
            definedAfterStatement(statement, state, ctx);
        }
        return state;
    }
    //=============================================================================
    // Marks bare reads that no assignment of the function can reach.
    void
    markUndefinedReads(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr code,
        const stringVector& inputs)
    {
        DefinednessContext ctx;
        ctx.lineUsages = &lineUsages;
        DefinedState entry;
        entry.names.insert(inputs.begin(), inputs.end());
        definedAfterBlock(code, entry, ctx);
    }
    //=============================================================================
    void
    applyAstLineUsageFromParserContext(const ParserContext& parserContext,
        const stringVector& lines, std::vector<CodeAnalyzerLineUsage>& lineUsages)
    {
        applyAstLineUsage(parserContext.ast, lineUsages, lines);
        // A global or persistent variable is defined outside the loop.
        std::set<std::string> declared;
        for (const CodeAnalyzerLineUsage& usage : lineUsages) {
            declared.insert(usage.globalDeclarations.begin(), usage.globalDeclarations.end());
            declared.insert(
                usage.persistentDeclarations.begin(), usage.persistentDeclarations.end());
        }
        LoopGrowthState scriptState;
        scriptState.definedVariables = declared;
        markLoopGrowth(lineUsages, lines, parserContext.ast, scriptState, 0);
        markUnreachableCode(lineUsages, lines, parserContext.ast);
        for (MacroFunctionDef* function = parserContext.macroFunctionDef; function != nullptr;
            function = function->nextFunction) {
            applyAstLineUsage(function->code, lineUsages, lines);
            LoopGrowthState functionState;
            functionState.definedVariables = declared;
            functionState.definedVariables.insert(
                function->arguments.begin(), function->arguments.end());
            markLoopGrowth(lineUsages, lines, function->code, functionState, 0);
            markUnreachableCode(lineUsages, lines, function->code);
            markDeadAssignments(lineUsages, function->code, function->returnVals);
            markUndefinedReads(lineUsages, function->code, function->arguments);
        }
    }
    //=============================================================================
    bool
    applyAstLineUsageFromSourceAt(const std::string& source, size_t firstLine,
        const stringVector& lines, std::vector<CodeAnalyzerLineUsage>& lineUsages)
    {
        // Blank lines in front keep the parsed line numbers equal to the file line numbers.
        LexerContext lexerContext;
        ParserContext context
            = parseStringResult(lexerContext, std::string(firstLine - 1, '\n') + source);
        if (context.state() == ParseError) {
            return false;
        }
        applyAstLineUsageFromParserContext(context, lines, lineUsages);
        return true;
    }
    //=============================================================================
    void
    applyAstLineUsageFromClassdef(const stringVector& lines,
        const std::vector<CodeAnalyzerFunction>& functions,
        const std::vector<CodeAnalyzerClassdef>& classdefs,
        std::vector<CodeAnalyzerLineUsage>& lineUsages)
    {
        // Each top-level function of the file (methods and local functions) is parsed alone.
        for (const CodeAnalyzerFunction& function : functions) {
            if (function.parentLine != 0 || function.line <= 0 || function.endLine < function.line
                || function.endLine > static_cast<int>(lines.size())) {
                continue;
            }
            std::string source;
            for (int k = function.line; k <= function.endLine; ++k) {
                source += lines[static_cast<size_t>(k - 1)] + '\n';
            }
            // A get.Name or set.Name accessor is renamed with the same length to parse.
            const size_t dot = function.name.find('.');
            if (dot != std::string::npos) {
                const size_t at = source.find(function.name);
                if (at != std::string::npos) {
                    source[at + dot] = '_';
                }
            }
            if (!applyAstLineUsageFromSourceAt(
                    source, static_cast<size_t>(function.line), lines, lineUsages)) {
                // Without a parsed body the dataflow rules would only report noise.
                if (CodeAnalyzerLineUsage* usage = usageAt(lineUsages, function.line)) {
                    usage->astUnavailable = true;
                }
            }
        }
        // A property default value is an expression that can call local functions.
        for (const CodeAnalyzerClassdef& classdef : classdefs) {
            for (const CodeAnalyzerClassProperty& property : classdef.properties) {
                if (property.line <= 0 || property.line > static_cast<int>(lines.size())) {
                    continue;
                }
                const std::string code
                    = stripComment(lines[static_cast<size_t>(property.line - 1)]);
                const size_t equals = codeAnalyzerAssignmentOperator(code);
                if (equals != std::string::npos) {
                    applyAstLineUsageFromSourceAt(code.substr(equals + 1) + '\n',
                        static_cast<size_t>(property.line), lines, lineUsages);
                }
            }
        }
    }
    //=============================================================================
    CodeAnalyzerFunction
    makeScriptScope(const stringVector& lines)
    {
        CodeAnalyzerFunction scope;
        scope.name = "<script>";
        scope.line = 0;
        scope.endLine = static_cast<int>(lines.size());
        scope.parentLine = -1;
        scope.script = true;
        return scope;
    }
    //=============================================================================
} // namespace
//=============================================================================
CodeAnalyzerAstModel
buildCodeAnalyzerAstModel(const stringVector& lines, const ParserContext& parserContext)
{
    CodeAnalyzerAstModel model;
    model.functions = buildCodeAnalyzerFunctionModel(lines, parserContext.macroFunctionDef);
    model.classdefs = buildCodeAnalyzerClassdefModel(lines);
    if (parserContext.state() == ScriptBlock) {
        model.functions.insert(model.functions.begin(), makeScriptScope(lines));
    }
    for (size_t k = 0; k < lines.size(); ++k) {
        model.lineUsages.push_back(collectCodeAnalyzerLineUsage(lines, static_cast<int>(k + 1)));
    }
    if (parserContext.state() == ClassDef) {
        applyAstLineUsageFromClassdef(lines, model.functions, model.classdefs, model.lineUsages);
    } else {
        applyAstLineUsageFromParserContext(parserContext, lines, model.lineUsages);
    }
    return model;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
