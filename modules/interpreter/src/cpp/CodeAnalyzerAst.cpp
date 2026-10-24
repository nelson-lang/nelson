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
#include <algorithm>
#include <set>
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
        appendUnique(callable ? usage->callableUsed : usage->used, node->text);
    }
    //=============================================================================
    void
    markExpressionUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node);
    //=============================================================================
    void
    markPostfixUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr postfix)
    {
        for (AbstractSyntaxTreePtr current = postfix; current != nullptr;
            current = current->right) {
            if (current->opNum == OP_PARENS || current->opNum == OP_BRACES
                || current->opNum == OP_DOTDYN) {
                markExpressionUsage(lineUsages, current->down);
            }
        }
    }
    //=============================================================================
    void
    markExpressionUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            AbstractSyntaxTreePtr expression = unwrapRhs(current);
            if (expression != current) {
                markExpressionUsage(lineUsages, expression);
                continue;
            }
            if (current->type == id_node) {
                const bool parenReference
                    = current->down != nullptr && current->down->opNum == OP_PARENS;
                appendAstUsage(lineUsages, current, parenReference);
                markPostfixUsage(lineUsages, current->down);
                continue;
            }
            if (current->type != non_terminal) {
                continue;
            }
            if (current->opNum == OP_DOT) {
                continue;
            }
            markExpressionUsage(lineUsages, current->down);
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
        collectAssignmentTarget(lineUsages, *usage, node->down);
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
        }
        markExpressionUsage(lineUsages, loopVariable->down);
    }
    //=============================================================================
    void
    markSwitchUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != reserved_node
            || node->tokenNumber != NLS_KEYWORD_SWITCH) {
            return;
        }
        markExpressionUsage(lineUsages, node->down);
    }
    //=============================================================================
    void
    markCaseUsage(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        if (node == nullptr || node->type != reserved_node
            || node->tokenNumber != NLS_KEYWORD_CASE) {
            return;
        }
        markExpressionUsage(lineUsages, node->down);
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
            for (int line = span.line - 1; line > 0; --line) {
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
        usage->used.erase(
            std::remove(usage->used.begin(), usage->used.end(), node->text), usage->used.end());
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
    bool
    isFlowBlockStartKeyword(unsigned int tokenNumber)
    {
        return tokenNumber == NLS_KEYWORD_IF || tokenNumber == NLS_KEYWORD_FOR
            || tokenNumber == NLS_KEYWORD_WHILE || tokenNumber == NLS_KEYWORD_SWITCH
            || tokenNumber == NLS_KEYWORD_TRY;
    }
    //=============================================================================
    void
    markFlowBlockStart(std::vector<CodeAnalyzerLineUsage>& lineUsages, AbstractSyntaxTreePtr node)
    {
        AstSourceSpan span = firstSourceSpan(node);
        CodeAnalyzerLineUsage* usage = usageAt(lineUsages, span.line);
        if (usage == nullptr) {
            return;
        }
        usage->flowBlockStart = true;
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
    applyAstLineUsage(AbstractSyntaxTreePtr node, std::vector<CodeAnalyzerLineUsage>& lineUsages,
        const stringVector& lines)
    {
        for (AbstractSyntaxTreePtr current = node; current != nullptr; current = current->right) {
            markAssignment(lineUsages, current);
            if (current->opNum == OP_ASSIGN) {
                markExpressionUsage(lineUsages, rightOperand(current));
                applyAstLineUsage(rightOperand(current), lineUsages, lines);
                continue;
            }
            markMultiCallAssignment(lineUsages, current);
            if (current->opNum == OP_MULTICALL) {
                markExpressionUsage(lineUsages, rightOperand(current));
                applyAstLineUsage(rightOperand(current), lineUsages, lines);
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
                markExpressionUsage(
                    lineUsages, current->down == nullptr ? nullptr : current->down->down);
                markIfConstantConditions(lineUsages, lines, current->down);
            }
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_WHILE) {
                markExpressionUsage(lineUsages, current->down);
                markConstantCondition(lineUsages, lines, current->down);
            }
            if (current->type == reserved_node && current->tokenNumber == NLS_KEYWORD_FOR) {
                markForLoopUsage(lineUsages, current);
            }
            markSwitchUsage(lineUsages, current);
            markCaseUsage(lineUsages, current);
            markCatchVariable(lineUsages, lines, current);
            if (current->type == reserved_node && isFlowBlockStartKeyword(current->tokenNumber)) {
                markFlowBlockStart(lineUsages, current);
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
    void
    applyAstLineUsageFromParserContext(const ParserContext& parserContext,
        const stringVector& lines, std::vector<CodeAnalyzerLineUsage>& lineUsages)
    {
        applyAstLineUsage(parserContext.ast, lineUsages, lines);
        for (MacroFunctionDef* function = parserContext.macroFunctionDef; function != nullptr;
            function = function->nextFunction) {
            applyAstLineUsage(function->code, lineUsages, lines);
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
    applyAstLineUsageFromParserContext(parserContext, lines, model.lineUsages);
    return model;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
