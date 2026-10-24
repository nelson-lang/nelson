//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <cctype>
#include <cstring>
#include <vector>
#include "NelsonParserHelpers.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefParserNative.hpp"
#include "LexerInterface.hpp"
#include "ParserInterface.hpp"
#include "FileParser.hpp"
#include "ParserState.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "NelsonFormattingTokens.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static thread_local ParserContext lastParserContext;
static thread_local ParserContext* activeParserContext = &lastParserContext;
//=============================================================================
static ParserContext&
currentParserContext()
{
    return *activeParserContext;
}
//=============================================================================
static ParserContext
parseStringWithContext(LexerContext& lexerContext, const std::string& txt, bool rethrowException,
    bool allowMixedScript);
static std::string
rewriteMetaclassQuerySyntax(const std::string& txt);
//=============================================================================
class ActiveParserContextScope
{
public:
    explicit ActiveParserContextScope(ParserContext& parserContext) : _previous(activeParserContext)
    {
        activeParserContext = &parserContext;
        resetParser();
    }

    ~ActiveParserContextScope() { activeParserContext = _previous; }

    ActiveParserContextScope(const ActiveParserContextScope&) = delete;
    ActiveParserContextScope&
    operator=(const ActiveParserContextScope&)
        = delete;

private:
    ParserContext* _previous;
};
//=============================================================================
static void
chainFunction(MacroFunctionDef* r)
{
    ParserContext& parserContext = currentParserContext();
    parserContext.parserState = FuncDef;
    r->nextFunction = nullptr;
    r->prevFunction = nullptr;
    if (parserContext.macroFunctionDef == nullptr) {
        parserContext.macroFunctionDef = r;
    } else {
        r->localFunction = true;
        r->nextFunction = parserContext.macroFunctionDef->nextFunction;
        if (r->nextFunction != nullptr) {
            r->nextFunction->prevFunction = r;
        }
        parserContext.macroFunctionDef->nextFunction = r;
        r->prevFunction = parserContext.macroFunctionDef;
    }
}
//=============================================================================
static void
getLinePosition(const ParseRHS& val, int& lineNumber, int& columnNumber)
{
    int tokenID = 0;
    if (val.isToken) {
        tokenID = val.v.i;
    } else if (val.v.p != nullptr) {
        tokenID = val.v.p->getContext();
    }
    lineNumber = tokenID & 0xFFFF;
    columnNumber = tokenID >> 16;
}
//=============================================================================
int
yyxpt(const std::string& xStr, const ParseRHS& val)
{
    int linenumber = 0;
    int colnumber = 0;
    getLinePosition(val, linenumber, colnumber);
    std::string msg;
    if (getParserFilenameU() == "") {
        msg = formatI18nMessage(
            resolveMessageTemplate("Nelson:interpreter:expectingS", "Expecting %s"), xStr);
    } else {
        msg = formatI18nMessage(
            resolveMessageTemplate("Nelson:interpreter:expectingAtLineColumnFile",
                "Expecting %s\n\tat line %d, column %d of file %s"),
            xStr, linenumber, colnumber, getParserFilenameU());
    }
    setParserDiagnostic(msg, linenumber, colnumber);
    raiseParserError(msg, "Nelson:interpreter:syntaxError");
}
//=============================================================================
int
yyxpt(const std::string& xStr)
{
    std::string msg;
    if (getParserFilenameU() == "") {
        msg = formatI18nMessage(
            resolveMessageTemplate("Nelson:interpreter:expectingS", "Expecting %s"), xStr);
    } else {
        msg = formatI18nMessage(
            resolveMessageTemplate("Nelson:interpreter:expectingAtLineColumnFile",
                "Expecting %s\n\tat line %d, column %d of file %s"),
            xStr, 0, 0, getParserFilenameU());
    }
    setParserDiagnostic(msg, 0, 0);
    raiseParserError(msg, "Nelson:interpreter:syntaxError");
}
//=============================================================================
void
raiseParserError(const std::string& message, const std::string& identifier)
{
    // RaiseError adds the call stack but returns when there is no main evaluator.
    RaiseError(message, identifier);
    throw NelsonException(message, identifier);
}
//=============================================================================
// Handles a bracketed left-hand side assignment: '[' matrixDef ']' '=' expr.
// With a single, non comma-list target it behaves as a plain 'target = expr'
// assignment (any right-hand side expression is allowed, matching expected behavior
// behaviour). Otherwise it is a multiple-output function call and the right-hand
// side must be a call form (identifier optionally followed by indexing).
AbstractSyntaxTreePtr
assignOrMultiFunctionCall(AbstractSyntaxTreePtr matrixDef, AbstractSyntaxTreePtr rhs,
    int bracketContext, int assignContext)
{
    int targetCount = 0;
    AbstractSyntaxTreePtr onlyTarget = nullptr;
    if (matrixDef != nullptr) {
        AbstractSyntaxTreePtr row = matrixDef->down;
        while (row != nullptr) {
            AbstractSyntaxTreePtr element = row->down;
            while (element != nullptr) {
                ++targetCount;
                onlyTarget = element;
                element = element->right;
            }
            row = row->right;
        }
    }
    bool singleSimpleTarget = false;
    if (targetCount == 1 && onlyTarget != nullptr && onlyTarget->type == non_terminal
        && onlyTarget->opNum == OP_RHS && onlyTarget->down != nullptr
        && onlyTarget->down->type == id_node && !isPlaceholderIdentifier(onlyTarget->down->text)) {
        // A brace or dotted reference on the target may expand to a comma-list, so
        // it stays a multiple-output assignment; parenthesized indexing does not.
        bool commaListReference = false;
        AbstractSyntaxTreePtr reference = onlyTarget->down->down;
        while (reference != nullptr) {
            if (reference->opNum == OP_BRACES || reference->opNum == OP_DOT
                || reference->opNum == OP_DOTDYN) {
                commaListReference = true;
                break;
            }
            reference = reference->right;
        }
        singleSimpleTarget = !commaListReference;
    }
    if (singleSimpleTarget) {
        AbstractSyntaxTreePtr lhs = onlyTarget->down;
        lhs->right = nullptr;
        return AbstractSyntaxTree::createNode(OP_ASSIGN, lhs, rhs, assignContext);
    }
    AbstractSyntaxTreePtr callee = nullptr;
    if (rhs != nullptr && rhs->type == non_terminal && rhs->opNum == OP_RHS && rhs->down != nullptr
        && rhs->down->type == id_node) {
        callee = rhs->down;
        callee->right = nullptr;
        if (callee->down == nullptr) {
            callee->addChild(AbstractSyntaxTree::createNode(OP_PARENS, nullptr, -1));
        }
    }
    if (callee == nullptr) {
        yyxpt(Nelson::resolveMessageTemplate("Nelson:interpreter:functionCallRightHand",
            "a function call on the right hand side of a multiple assignment"));
        return nullptr;
    }
    return AbstractSyntaxTree::createNode(OP_MULTICALL, matrixDef, callee, bracketContext);
}
//=============================================================================
void
functionBody(const ParseRHS& lhsRhs, const ParseRHS& nameRhs, const ParseRHS& rhsRhs,
    const ParseRHS& codeRhs)
{
    auto* r = new MacroFunctionDef();
    if (lhsRhs.v.p != nullptr) {
        r->returnVals = lhsRhs.v.p->toStringList();
    }
    if (nameRhs.v.p != nullptr) {
        r->setName(nameRhs.v.p->text);
    }
    if (rhsRhs.v.p != nullptr) {
        r->arguments = rhsRhs.v.p->toStringList();
    }
    if (codeRhs.v.p != nullptr) {
        r->code = codeRhs.v.p;
    }
    r->setFilename(getParserFilenameW());
    chainFunction(r);
}
//=============================================================================
std::string
decodeline(const ParseRHS& val)
{
    int linenumber = 0;
    int colnumber = 0;
    getLinePosition(val, linenumber, colnumber);
    return fmt::to_string(linenumber);
}
//=============================================================================
void
resetParser()
{
    currentParserContext() = ParserContext {};
}
//=============================================================================
void
setParserDiagnostic(const std::string& message, int line, int column)
{
    ParserContext& parserContext = currentParserContext();
    parserContext.parserState = ParseError;
    parserContext.diagnostic.message = message;
    parserContext.diagnostic.filename = getParserFilenameU();
    parserContext.diagnostic.line = line;
    parserContext.diagnostic.column = column;
}
//=============================================================================
static void
setParserDiagnosticIfMissing(ParserContext& parserContext, const std::string& message)
{
    if (parserContext.diagnostic.message.empty()) {
        setParserDiagnostic(message, 0, 0);
    }
}
//=============================================================================
static bool
isMetaclassIdentifierStart(char ch)
{
    return std::isalpha(static_cast<unsigned char>(ch)) != 0 || ch == '_';
}
//=============================================================================
static bool
isMetaclassIdentifierPart(char ch)
{
    return std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_';
}
//=============================================================================
static bool
isLikelySingleQuotedTextStart(const std::string& txt, size_t quotePosition)
{
    size_t previous = quotePosition;
    while (previous > 0) {
        const char ch = txt[previous - 1];
        if (ch != ' ' && ch != '\t' && ch != '\r') {
            break;
        }
        --previous;
    }
    if (previous == 0) {
        return true;
    }

    const char ch = txt[previous - 1];
    if (std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_' || ch == ')' || ch == ']'
        || ch == '}') {
        return false;
    }
    if (ch == '.') {
        return false;
    }
    return true;
}
//=============================================================================
std::vector<std::pair<size_t, size_t>>
findMetaclassQueryRanges(const std::string& txt)
{
    enum class ScanState
    {
        Normal,
        SingleQuotedText,
        DoubleQuotedText,
        LineComment,
        BlockComment
    };

    std::vector<std::pair<size_t, size_t>> ranges;
    ScanState state = ScanState::Normal;
    for (size_t k = 0; k < txt.size();) {
        const char ch = txt[k];
        const char next = k + 1 < txt.size() ? txt[k + 1] : '\0';
        ++k;
        if (state == ScanState::LineComment) {
            if (ch == '\n') {
                state = ScanState::Normal;
            }
        } else if (state == ScanState::BlockComment) {
            if (ch == '%' && next == '}') {
                ++k;
                state = ScanState::Normal;
            }
        } else if (state == ScanState::SingleQuotedText || state == ScanState::DoubleQuotedText) {
            const char quote = state == ScanState::SingleQuotedText ? '\'' : '"';
            if (ch == quote && next == quote) {
                ++k;
            } else if (ch == quote) {
                state = ScanState::Normal;
            }
        } else if (ch == '%') {
            state = next == '{' ? ScanState::BlockComment : ScanState::LineComment;
            k += next == '{' ? 1 : 0;
        } else if (ch == '\'' && isLikelySingleQuotedTextStart(txt, k - 1)) {
            state = ScanState::SingleQuotedText;
        } else if (ch == '"') {
            state = ScanState::DoubleQuotedText;
        } else if (ch == '?' && isMetaclassIdentifierStart(next)) {
            size_t end = k + 1;
            while (end < txt.size()) {
                while (end < txt.size() && isMetaclassIdentifierPart(txt[end])) {
                    ++end;
                }
                if (end + 1 >= txt.size() || txt[end] != '.'
                    || !isMetaclassIdentifierStart(txt[end + 1])) {
                    break;
                }
                end += 2;
            }
            ranges.emplace_back(k - 1, end);
            k = end;
        }
    }
    return ranges;
}
//=============================================================================
static std::string
rewriteMetaclassQuerySyntax(const std::string& txt)
{
    std::string result;
    result.reserve(txt.size());
    size_t copied = 0;
    for (const auto& range : findMetaclassQueryRanges(txt)) {
        result.append(txt, copied, range.first - copied);
        result += "metaclass('";
        result.append(txt, range.first + 1, range.second - range.first - 1);
        result += "')";
        copied = range.second;
    }
    result.append(txt, copied, std::string::npos);
    return result;
}
//=============================================================================
static bool
lineStartsWithFunctionKeyword(const std::string& txt, size_t lineStart, size_t lineEnd)
{
    size_t pos = lineStart;
    while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
        pos++;
    }
    static constexpr const char* functionKeyword = "function";
    static constexpr size_t functionKeywordLength = 8;
    if ((lineEnd - pos) < functionKeywordLength) {
        return false;
    }
    if (txt.compare(pos, functionKeywordLength, functionKeyword) != 0) {
        return false;
    }
    const size_t after = pos + functionKeywordLength;
    if (after >= lineEnd) {
        return true;
    }
    const unsigned char next = static_cast<unsigned char>(txt[after]);
    return (std::isspace(next) != 0);
}
//=============================================================================
static bool
lineHasScriptContent(const std::string& txt, size_t lineStart, size_t lineEnd)
{
    size_t pos = lineStart;
    while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
        pos++;
    }
    return pos < lineEnd;
}
//=============================================================================
static void
appendFunctionChain(MacroFunctionDef* head, MacroFunctionDef* toAppend, bool markNested)
{
    if (head == nullptr || toAppend == nullptr) {
        return;
    }
    MacroFunctionDef* tail = head;
    while (tail->nextFunction != nullptr) {
        tail = tail->nextFunction;
    }
    MacroFunctionDef* cp = toAppend;
    bool first = true;
    while (cp != nullptr) {
        MacroFunctionDef* next = cp->nextFunction;
        cp->localFunction = true;
        cp->nestedFunction = markNested;
        if (first) {
            cp->prevFunction = head;
            first = false;
        }
        tail->nextFunction = cp;
        tail = cp;
        cp = next;
    }
}
//=============================================================================
static std::string
replaceRangesWithNewlines(
    const std::string& txt, const std::vector<std::pair<size_t, size_t>>& ranges)
{
    std::string result = txt;
    for (const auto& range : ranges) {
        for (size_t k = range.first; k < range.second && k < result.size(); ++k) {
            if (result[k] != '\n' && result[k] != '\r') {
                result[k] = ' ';
            }
        }
    }
    return result;
}
//=============================================================================
static bool
findLineCodeStart(
    const std::string& txt, size_t lineStart, size_t lineEnd, bool& inMultilineComment, size_t& pos)
{
    pos = lineStart;
    while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
        pos++;
    }
    if (inMultilineComment) {
        size_t endComment = txt.find("%}", pos);
        if (endComment == std::string::npos || endComment > lineEnd) {
            return false;
        }
        inMultilineComment = false;
        pos = endComment + 2;
        while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
            pos++;
        }
    }
    if (pos >= lineEnd) {
        return false;
    }
    if (txt.compare(pos, 2, "%{") == 0) {
        size_t endComment = txt.find("%}", pos + 2);
        if (endComment == std::string::npos || endComment > lineEnd) {
            inMultilineComment = true;
            return false;
        }
        pos = endComment + 2;
        while (pos < lineEnd && ((txt[pos] == ' ') || (txt[pos] == '\t'))) {
            pos++;
        }
    }
    return pos < lineEnd && txt[pos] != '%';
}
//=============================================================================
// Classify an identifier occupying [start, end) of txt as a block-structure keyword.
// 0 = none, +1 = block opener (function/if/for/while/switch/try/arguments), -1 = 'end'.
static int
blockKeywordDelta(const std::string& txt, size_t start, size_t end)
{
    const size_t len = end - start;
    auto is = [&](const char* w, size_t wl) { return len == wl && txt.compare(start, wl, w) == 0; };
    if (is("end", 3)) {
        return -1;
    }
    if (is("if", 2) || is("for", 3) || is("while", 5) || is("switch", 6) || is("try", 3)
        || is("function", 8) || is("arguments", 9)) {
        return 1;
    }
    return 0;
}
//=============================================================================
// Append to `identifiers` the [start, end) ranges of every identifier that appears at
// statement level in the code region [codeStart, lineEnd): bracket depth 0, not a field
// access (after '.'), and outside strings/transpose/mid-line comments. Shared tokenizer
// behind the block-structure scanners, so that 'end' used as an index (inside () [] {}) or
// text inside a string is never mistaken for a block keyword.
static void
collectStatementLevelIdentifiers(const std::string& txt, size_t codeStart, size_t lineEnd,
    std::vector<std::pair<size_t, size_t>>& identifiers)
{
    size_t i = codeStart;
    int bracketDepth = 0;
    while (i < lineEnd) {
        const char c = txt[i];
        if (c == '%') {
            break;
        }
        if (c == '"') {
            for (++i; i < lineEnd; ++i) {
                if (txt[i] == '"') {
                    if (i + 1 < lineEnd && txt[i + 1] == '"') {
                        ++i;
                        continue;
                    }
                    ++i;
                    break;
                }
            }
            continue;
        }
        if (c == '\'') {
            if (isLikelySingleQuotedTextStart(txt, i)) {
                for (++i; i < lineEnd; ++i) {
                    if (txt[i] == '\'') {
                        if (i + 1 < lineEnd && txt[i + 1] == '\'') {
                            ++i;
                            continue;
                        }
                        ++i;
                        break;
                    }
                }
            } else {
                ++i;
            }
            continue;
        }
        if (c == '(' || c == '[' || c == '{') {
            bracketDepth++;
            ++i;
            continue;
        }
        if (c == ')' || c == ']' || c == '}') {
            if (bracketDepth > 0) {
                bracketDepth--;
            }
            ++i;
            continue;
        }
        if ((std::isalpha(static_cast<unsigned char>(c)) != 0) || c == '_') {
            size_t j = i;
            while (j < lineEnd && isMetaclassIdentifierPart(txt[j])) {
                ++j;
            }
            const bool afterDot = (i > 0 && txt[i - 1] == '.');
            if (bracketDepth == 0 && !afterDot) {
                identifiers.emplace_back(i, j);
            }
            i = j;
            continue;
        }
        ++i;
    }
}
//=============================================================================
// Find the ranges of every nested function (a function defined inside the body of the
// first, parent function). Scans keyword by keyword so that a nested one-line function
// 'function g(v), g = v; end' is detected as well as the multi-line form.
static bool
findNestedFunctionBlocks(const std::string& txt, std::vector<std::pair<size_t, size_t>>& ranges)
{
    bool sawFirstFunction = false;
    bool inMultilineComment = false;
    int blockDepth = 0;
    size_t nestedStart = std::string::npos;
    size_t lineStart = 0;
    while (lineStart < txt.size()) {
        size_t lineEnd = txt.find('\n', lineStart);
        if (lineEnd == std::string::npos) {
            lineEnd = txt.size();
        }
        const size_t nextLineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
        size_t pos = 0;
        if (!findLineCodeStart(txt, lineStart, lineEnd, inMultilineComment, pos)) {
            lineStart = nextLineStart;
            continue;
        }
        std::vector<std::pair<size_t, size_t>> ids;
        collectStatementLevelIdentifiers(txt, pos, lineEnd, ids);
        for (const auto& id : ids) {
            const bool isFunctionKw
                = ((id.second - id.first) == 8 && txt.compare(id.first, 8, "function") == 0);
            const int delta = blockKeywordDelta(txt, id.first, id.second);
            if (isFunctionKw) {
                if (!sawFirstFunction) {
                    sawFirstFunction = true;
                    blockDepth = 1;
                } else {
                    if (blockDepth == 1 && nestedStart == std::string::npos) {
                        nestedStart = lineStart;
                    }
                    blockDepth++;
                }
            } else if (delta > 0) {
                if (sawFirstFunction) {
                    blockDepth++;
                }
            } else if (delta < 0) {
                if (sawFirstFunction && blockDepth > 0) {
                    blockDepth--;
                    if (nestedStart != std::string::npos && blockDepth == 1) {
                        ranges.emplace_back(nestedStart, nextLineStart);
                        nestedStart = std::string::npos;
                    }
                }
            }
        }
        lineStart = nextLineStart;
    }
    return !ranges.empty();
}
//=============================================================================
static bool
tryParseFunctionWithNestedFunctions(
    LexerContext& lexerContext, ParserContext& parserContext, const std::string& txt)
{
    std::vector<std::pair<size_t, size_t>> nestedRanges;
    if (!findNestedFunctionBlocks(txt, nestedRanges)) {
        return false;
    }
    const std::string parentSource = replaceRangesWithNewlines(txt, nestedRanges);
    const std::string originalFilename = getParserFilenameU();
    const std::wstring originalFilenameW = getParserFilenameW();

    LexerContext parentLexerContext;
    ParserContext parentContext
        = parseStringWithContext(parentLexerContext, parentSource, false, false);
    setParserFilename(originalFilename);
    if (parentContext.state() != FuncDef || parentContext.macroFunctionDef == nullptr) {
        return false;
    }

    for (const auto& range : nestedRanges) {
        // Blank everything around the nested function so that its offsets, lines and
        // columns (diagnostics, formatting tokens) are those of the original source.
        std::string nestedSource
            = replaceRangesWithNewlines(txt, { { 0, range.first }, { range.second, txt.size() } });
        if (!nestedSource.empty() && nestedSource.back() != '\n') {
            nestedSource += '\n';
        }
        LexerContext nestedLexerContext;
        ParserContext nestedContext
            = parseStringWithContext(nestedLexerContext, nestedSource, false, true);
        setParserFilename(originalFilename);
        if (nestedContext.state() != FuncDef || nestedContext.macroFunctionDef == nullptr) {
            return false;
        }
        if (!originalFilenameW.empty()) {
            MacroFunctionDef* cp = nestedContext.macroFunctionDef;
            while (cp != nullptr) {
                cp->setFilename(originalFilenameW);
                cp = cp->nextFunction;
            }
        }
        appendFunctionChain(parentContext.macroFunctionDef, nestedContext.macroFunctionDef, true);
    }
    parserContext = parentContext;
    return true;
}
//=============================================================================
// Walk the code region [codeStart, lineEnd) of a single line and update the running block
// state, so that a one-line 'function ... end' opens and closes on the same line. A
// top-level function block only opens when 'function' is the first code token of the line
// (functionStart set to lineStart); nested blocks just move `depth`.
static void
scanLineBlockStructure(const std::string& txt, size_t codeStart, size_t lineEnd, size_t lineStart,
    size_t nextLineStart, int& depth, size_t& functionStart,
    std::vector<std::pair<size_t, size_t>>& functionRanges)
{
    std::vector<std::pair<size_t, size_t>> ids;
    collectStatementLevelIdentifiers(txt, codeStart, lineEnd, ids);
    for (const auto& id : ids) {
        const int delta = blockKeywordDelta(txt, id.first, id.second);
        const bool isFunctionKw
            = ((id.second - id.first) == 8 && txt.compare(id.first, 8, "function") == 0);
        if (functionStart == std::string::npos) {
            if (isFunctionKw && depth == 0 && id.first == codeStart) {
                functionStart = lineStart;
                depth = 1;
            } else if (delta > 0) {
                depth++;
            } else if (delta < 0 && depth > 0) {
                depth--;
            }
        } else {
            if (delta > 0) {
                depth++;
            } else if (delta < 0 && depth > 0) {
                depth--;
                if (depth == 0) {
                    functionRanges.emplace_back(functionStart, nextLineStart);
                    functionStart = std::string::npos;
                }
            }
        }
    }
}
//=============================================================================
// Collect every top-level (depth 0) 'function ... end' block range in the source.
// A function keyword only opens a collected block when the scanner is at depth 0 and
// outside any already-open function block; functions nested inside a control-flow block
// (if/for/while/switch/try/arguments) are therefore left in place, matching the rule that
// local functions may be interleaved with top-level script statements but may not be
// defined inside a control-flow block. Returns true only when at least one complete
// top-level function was found with no dangling (unterminated) function block.
static bool
collectTopLevelFunctionRanges(const std::string& txt,
    std::vector<std::pair<size_t, size_t>>& functionRanges, bool& hasScriptContentOutsideFunctions)
{
    functionRanges.clear();
    hasScriptContentOutsideFunctions = false;
    bool inMultilineComment = false;
    int depth = 0;
    bool haveScriptContent = false;
    size_t functionStart = std::string::npos;
    size_t lineStart = 0;
    while (lineStart < txt.size()) {
        size_t lineEnd = txt.find('\n', lineStart);
        if (lineEnd == std::string::npos) {
            lineEnd = txt.size();
        }
        const size_t nextLineStart = (lineEnd < txt.size()) ? lineEnd + 1 : txt.size();
        size_t pos = 0;
        if (!findLineCodeStart(txt, lineStart, lineEnd, inMultilineComment, pos)) {
            lineStart = nextLineStart;
            continue;
        }
        const bool atScriptLevel = (functionStart == std::string::npos);
        const bool firstTokenIsFunction = lineStartsWithFunctionKeyword(txt, lineStart, lineEnd);
        scanLineBlockStructure(
            txt, pos, lineEnd, lineStart, nextLineStart, depth, functionStart, functionRanges);
        if (atScriptLevel && !firstTokenIsFunction && lineHasScriptContent(txt, pos, lineEnd)) {
            haveScriptContent = true;
        }
        lineStart = nextLineStart;
    }
    hasScriptContentOutsideFunctions = haveScriptContent;
    return !functionRanges.empty() && functionStart == std::string::npos;
}
//=============================================================================
// Complement of a set of ordered, non-overlapping ranges over [0, total): the spans of
// the source that are NOT covered by any range.
static std::vector<std::pair<size_t, size_t>>
complementRanges(const std::vector<std::pair<size_t, size_t>>& ranges, size_t total)
{
    std::vector<std::pair<size_t, size_t>> result;
    size_t cursor = 0;
    for (const auto& range : ranges) {
        if (range.first > cursor) {
            result.emplace_back(cursor, range.first);
        }
        cursor = std::max(cursor, range.second);
    }
    if (cursor < total) {
        result.emplace_back(cursor, total);
    }
    return result;
}
//=============================================================================
// Extract the script name (base name without directory or extension) from a file path.
static std::string
scriptNameFromPath(const std::string& path)
{
    const size_t slash = path.find_last_of("/\\");
    std::string base = (slash == std::string::npos) ? path : path.substr(slash + 1);
    const size_t dot = base.find_last_of('.');
    if (dot != std::string::npos && dot != 0) {
        base = base.substr(0, dot);
    }
    return base;
}
//=============================================================================
// Cheap predicate: does the source interleave top-level script statements with one or
// more local function definitions? Used both to drive the mixed-script fallback and to
// route such files straight to it, skipping a grammar parse that is guaranteed to fail.
static bool
looksLikeMixedScriptLocalFunctions(const std::string& txt)
{
    if (txt.find("function") == std::string::npos) {
        return false;
    }
    std::vector<std::pair<size_t, size_t>> functionRanges;
    bool hasScriptContent = false;
    if (!collectTopLevelFunctionRanges(txt, functionRanges, hasScriptContent)) {
        return false;
    }
    return hasScriptContent;
}
//=============================================================================
static bool
tryParseMixedScriptLocalFunctions(
    LexerContext& lexerContext, ParserContext& parserContext, const std::string& txt)
{
    std::vector<std::pair<size_t, size_t>> functionRanges;
    bool hasScriptContent = false;
    if (!collectTopLevelFunctionRanges(txt, functionRanges, hasScriptContent)
        || !hasScriptContent) {
        return false;
    }
    const std::string originalFilename = getParserFilenameU();
    const std::wstring originalFilenameW = getParserFilenameW();
    // Both parts keep the original line numbering: replaceRangesWithNewlines only blanks
    // non-newline characters, so diagnostics still point at the right source lines. The
    // script part blanks out every function block; the function part blanks everything in
    // between, so top-level functions can appear anywhere among the script statements.
    const std::string scriptPart = replaceRangesWithNewlines(txt, functionRanges);
    const std::string functionPart
        = replaceRangesWithNewlines(txt, complementRanges(functionRanges, txt.size()));

    LexerContext scriptLexerContext;
    ParserContext scriptContext
        = parseStringWithContext(scriptLexerContext, scriptPart, false, false);
    setParserFilename(originalFilename);
    if (scriptContext.state() != ScriptBlock || scriptContext.ast == nullptr) {
        return false;
    }

    LexerContext functionLexerContext;
    // Parse the function part with the mixed-script fallbacks enabled so that a local
    // function containing a nested function is handled (tryParseFunctionWithNestedFunctions).
    // The function part never has leading script content, so it cannot re-trigger the
    // script/function split.
    ParserContext functionContext
        = parseStringWithContext(functionLexerContext, functionPart, false, true);
    setParserFilename(originalFilename);
    if (functionContext.state() != FuncDef || functionContext.macroFunctionDef == nullptr) {
        return false;
    }
    if (!originalFilenameW.empty()) {
        MacroFunctionDef* cp = functionContext.macroFunctionDef;
        while (cp != nullptr) {
            cp->setFilename(originalFilenameW);
            cp = cp->nextFunction;
        }
    }

    parserContext = ParserContext {};
    parserContext.ast = scriptContext.ast;
    parserContext.macroFunctionDef = functionContext.macroFunctionDef;
    parserContext.parserState = ScriptBlock;

    // A local function in a script may not share the script name (the file base name).
    // Only enforced when a filename is known (i.e. parsing a file, not a string).
    if (!originalFilename.empty()) {
        const std::string scriptName = scriptNameFromPath(originalFilename);
        if (!scriptName.empty()) {
            for (MacroFunctionDef* cp = functionContext.macroFunctionDef; cp != nullptr;
                cp = cp->nextFunction) {
                if (cp->getName() == scriptName) {
                    setParserDiagnostic(
                        formatI18nMessage(
                            resolveMessageTemplate("Nelson:interpreter:localFunctionNameConflict",
                                "Local function name '%s' must be different from the script name."),
                            scriptName),
                        0, 0);
                    parserContext.parserState = ParseError;
                    break;
                }
            }
        }
    }
    return true;
}
//=============================================================================
void
setParsedScriptBlock(AbstractSyntaxTreePtr ast)
{
    ParserContext& parserContext = currentParserContext();
    parserContext.ast = ast;
    parserContext.parserState = ScriptBlock;
}
//=============================================================================
void
setParsedFunctionDef(MacroFunctionDef* r)
{
    ParserContext& parserContext = currentParserContext();
    parserContext.macroFunctionDef = r;
    parserContext.parserState = FuncDef;
}
//=============================================================================
AbstractSyntaxTreePtr
getParsedScriptBlock()
{
    return currentParserContext().ast;
}
//=============================================================================
MacroFunctionDef*
getParsedFunctionDef()
{
    return currentParserContext().macroFunctionDef;
}
//=============================================================================
ParserState
parseState()
{
    return currentParserContext().state();
}
//=============================================================================
static size_t
formattingTokenMark()
{
    const std::vector<FormattingToken>* sink = getFormattingTokenSink();
    return sink == nullptr ? 0 : sink->size();
}
//=============================================================================
// A failed pass leaves the formatting tokens it scanned: drop them before another pass
// scans the same source again.
static void
discardFormattingTokensFrom(size_t mark)
{
    std::vector<FormattingToken>* sink = getFormattingTokenSink();
    if (sink != nullptr && sink->size() > mark) {
        sink->resize(mark);
    }
}
//=============================================================================
static bool
tryParseFallbacks(LexerContext& lexerContext, ParserContext& parserContext, const std::string& txt,
    size_t tokenMark)
{
    discardFormattingTokensFrom(tokenMark);
    if (tryParseFunctionWithNestedFunctions(lexerContext, parserContext, txt)) {
        return true;
    }
    discardFormattingTokensFrom(tokenMark);
    return tryParseMixedScriptLocalFunctions(lexerContext, parserContext, txt);
}
//=============================================================================
static ParserContext
parseStringWithContext(LexerContext& lexerContext, const std::string& txt, bool rethrowException,
    bool allowMixedScript)
{
    ParserContext parserContext;
    ActiveParserContextScope activeContext(parserContext);
    setParserFilename("");
    if (classdefLooksLikeSource(txt)) {
        ClassdefDefinition definition;
        std::string errorMessage;
        if (classdef_detail::parseClassdefSourceNativeFull(txt, definition, errorMessage)) {
            parserContext.parserState = ClassDef;
        } else {
            setParserDiagnosticIfMissing(parserContext, errorMessage);
        }
        lastParserContext = parserContext;
        return lastParserContext;
    }
    const std::string rewrittenText = rewriteMetaclassQuerySyntax(txt);
    // Fast path: a script that interleaves top-level local functions is always rejected by
    // the pure grammar, so route it straight to the mixed-script parser and avoid a
    // guaranteed-to-fail grammar parse plus exception unwinding. On the rare miss (the file
    // only looks mixed) we fall through to the grammar for an accurate diagnostic.
    const size_t tokenMark = formattingTokenMark();
    if (allowMixedScript && looksLikeMixedScriptLocalFunctions(rewrittenText)) {
        if (tryParseMixedScriptLocalFunctions(lexerContext, parserContext, rewrittenText)) {
            lastParserContext = parserContext;
            return lastParserContext;
        }
        discardFormattingTokensFrom(tokenMark);
    }
    setLexBuffer(lexerContext, rewrittenText);
    try {
        if (callyyparse(lexerContext, parserContext) != 0) {
            setParserDiagnosticIfMissing(parserContext,
                resolveMessageTemplate(
                    "Nelson:interpreter:unexpectedParserError", "Unexpected parser error."));
            if (allowMixedScript
                && tryParseFallbacks(lexerContext, parserContext, rewrittenText, tokenMark)) {
                lastParserContext = parserContext;
                return lastParserContext;
            }
        }
    } catch (const NelsonException& NelsonException) {
        setParserDiagnosticIfMissing(parserContext, wstring_to_utf8(NelsonException.message()));
        if (allowMixedScript
            && tryParseFallbacks(lexerContext, parserContext, rewrittenText, tokenMark)) {
            lastParserContext = parserContext;
            return lastParserContext;
        }
        lastParserContext = parserContext;
        if (rethrowException) {
            throw;
        }
        return lastParserContext;
    } catch (...) {
        setParserDiagnosticIfMissing(parserContext,
            resolveMessageTemplate(
                "Nelson:interpreter:unexpectedParserError", "Unexpected parser error."));
        if (allowMixedScript
            && tryParseFallbacks(lexerContext, parserContext, rewrittenText, tokenMark)) {
            lastParserContext = parserContext;
            return lastParserContext;
        }
        lastParserContext = parserContext;
        if (rethrowException) {
            throw;
        }
        return lastParserContext;
    }
    lastParserContext = parserContext;
    return lastParserContext;
}
//=============================================================================
ParserState
parseString(LexerContext& lexerContext, const std::string& txt)
{
    return parseStringWithContext(lexerContext, txt, true, true).state();
}
//=============================================================================
ParserContext
parseStringResult(LexerContext& lexerContext, const std::string& txt)
{
    return parseStringWithContext(lexerContext, txt, false, true);
}
//=============================================================================
static ParserContext
parseFileWithContext(
    LexerContext& lexerContext, FILE* fp, const std::string& fname, bool rethrowException)
{
    ParserContext parserContext;
    ActiveParserContextScope activeContext(parserContext);
    setParserFilename(fname);
    std::string fileContent;
    if (fp != nullptr) {
        char buffer[4096];
        while (fgets(buffer, sizeof(buffer), fp)) {
            fileContent += buffer;
        }
    }
    if (classdefLooksLikeSource(fileContent)) {
        ClassdefDefinition definition;
        std::string errorMessage;
        if (classdef_detail::parseClassdefSourceNativeFull(fileContent, definition, errorMessage)) {
            parserContext.parserState = ClassDef;
        } else {
            setParserDiagnosticIfMissing(parserContext, errorMessage);
        }
        setParserFilename("");
        lastParserContext = parserContext;
        return lastParserContext;
    }
    const std::string rewrittenFileContent = rewriteMetaclassQuerySyntax(fileContent);
    // Fast path (see parseStringWithContext): divert interleaved script/local-function
    // files to the mixed-script parser, skipping a grammar parse that cannot succeed.
    const size_t tokenMark = formattingTokenMark();
    if (looksLikeMixedScriptLocalFunctions(rewrittenFileContent)) {
        if (tryParseMixedScriptLocalFunctions(lexerContext, parserContext, rewrittenFileContent)) {
            setParserFilename("");
            lastParserContext = parserContext;
            return lastParserContext;
        }
        discardFormattingTokensFrom(tokenMark);
    }
    setLexBuffer(lexerContext, rewrittenFileContent);
    try {
        if (callyyparse(lexerContext, parserContext) != 0) {
            setParserDiagnosticIfMissing(parserContext,
                resolveMessageTemplate(
                    "Nelson:interpreter:unexpectedParserError", "Unexpected parser error."));
            if (tryParseFallbacks(lexerContext, parserContext, rewrittenFileContent, tokenMark)) {
                setParserFilename("");
                lastParserContext = parserContext;
                return lastParserContext;
            }
        }
    } catch (const NelsonException& NelsonException) {
        setParserDiagnosticIfMissing(parserContext, wstring_to_utf8(NelsonException.message()));
        if (tryParseFallbacks(lexerContext, parserContext, rewrittenFileContent, tokenMark)) {
            setParserFilename("");
            lastParserContext = parserContext;
            return lastParserContext;
        }
        setParserFilename("");
        lastParserContext = parserContext;
        if (rethrowException) {
            throw;
        }
        return lastParserContext;
    } catch (...) {
        setParserDiagnosticIfMissing(parserContext,
            resolveMessageTemplate(
                "Nelson:interpreter:unexpectedParserError", "Unexpected parser error."));
        if (tryParseFallbacks(lexerContext, parserContext, rewrittenFileContent, tokenMark)) {
            setParserFilename("");
            lastParserContext = parserContext;
            return lastParserContext;
        }
        setParserFilename("");
        lastParserContext = parserContext;
        if (rethrowException) {
            throw;
        }
        return lastParserContext;
    }
    setParserFilename("");
    lastParserContext = parserContext;
    return lastParserContext;
}
//=============================================================================
ParserState
parseFile(LexerContext& lexerContext, FILE* fp, const std::string& fname)
{
    return parseFileWithContext(lexerContext, fp, fname, true).state();
}
//=============================================================================
ParserContext
parseFileResult(LexerContext& lexerContext, FILE* fp, const std::string& fname)
{
    return parseFileWithContext(lexerContext, fp, fname, false);
}
//=============================================================================
}
//=============================================================================
