//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// clang-format off
//=============================================================================
// Compile-time lowering of "arguments" validation blocks.
//
// The generation helpers below are a faithful, self-contained copy of the
// pinned generator that historically lived in the pre-lexer text rewrite
// (NelsonLexer.lxx). Keeping the generator verbatim guarantees that the native
// front-end path emits the exact same validation statements, error strings and
// call-time (runtime) reporting as the legacy rewrite. The only new piece is the
// orchestrator lowerArgumentsBlocks(), which walks the arguments blocks captured
// natively by the grammar instead of scanning the whole source buffer.
//=============================================================================
#include <cctype>
#include <cstddef>
#include <string>
#include <vector>
#include "ArgumentsBlockLowering.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
namespace {
//=============================================================================
static std::string
trimLexerString(const std::string& text)
{
    size_t first = 0;
    while (first < text.size() && std::isspace(static_cast<unsigned char>(text[first]))) {
        first++;
    }
    size_t last = text.size();
    while (last > first && std::isspace(static_cast<unsigned char>(text[last - 1]))) {
        last--;
    }
    return text.substr(first, last - first);
}
//=============================================================================
static bool
startsWithLexerString(const std::string& text, const std::string& prefix)
{
    return text.compare(0, prefix.size(), prefix) == 0;
}
//=============================================================================
static bool
isIdentifierLexerChar(char ch)
{
    return std::isalnum(static_cast<unsigned char>(ch)) || ch == '_';
}
//=============================================================================
static size_t
findTopLevelChar(const std::string& text, char needle)
{
    int parenDepth = 0;
    int braceDepth = 0;
    int bracketDepth = 0;
    char stringDelimiter = '\0';
    size_t k = 0;
    while (k < text.size()) {
        const char ch = text[k];
        if (stringDelimiter != '\0') {
            if (ch == stringDelimiter) {
                if ((k + 1) < text.size() && text[k + 1] == stringDelimiter) {
                    k += 2; // escaped delimiter ("" or '')
                    continue;
                } else {
                    stringDelimiter = '\0';
                    k++;
                    continue;
                }
            }
            k++;
            continue;
        }
        if (ch == '\'' || ch == '"') {
            stringDelimiter = ch;
            k++;
            continue;
        }
        if (ch == '(') {
            parenDepth++;
        } else if (ch == ')' && parenDepth > 0) {
            parenDepth--;
        } else if (ch == '{') {
            braceDepth++;
        } else if (ch == '}' && braceDepth > 0) {
            braceDepth--;
        } else if (ch == '[') {
            bracketDepth++;
        } else if (ch == ']' && bracketDepth > 0) {
            bracketDepth--;
        } else if (ch == needle && parenDepth == 0 && braceDepth == 0 && bracketDepth == 0) {
            return k;
        }
        k++;
    }
    return std::string::npos;
}
//=============================================================================
static std::vector<std::string>
splitTopLevelCommaList(const std::string& text)
{
    std::vector<std::string> parts;
    size_t start = 0;
    for (;;) {
        const std::string remaining = text.substr(start);
        const size_t comma = findTopLevelChar(remaining, ',');
        if (comma == std::string::npos) {
            std::string part = trimLexerString(remaining);
            if (!part.empty()) {
                parts.push_back(part);
            }
            break;
        }
        std::string part = trimLexerString(remaining.substr(0, comma));
        if (!part.empty()) {
            parts.push_back(part);
        }
        start += comma + 1;
    }
    return parts;
}
//=============================================================================
static std::string
singleLineGeneratedCode(const std::string& text)
{
    std::string result;
    result.reserve(text.size());
    bool previousWasSpace = false;
    for (const char ch : text) {
        if (ch == '\r' || ch == '\n') {
            if (!previousWasSpace) {
                result.push_back(' ');
                previousWasSpace = true;
            }
            continue;
        }
        result.push_back(ch);
        previousWasSpace = std::isspace(static_cast<unsigned char>(ch)) != 0;
    }
    return trimLexerString(result);
}
//=============================================================================
// A single quote opens a character array unless it directly follows a value
// (an identifier char, a closing bracket, a dot or another quote), in which
// case it is the transpose operator.
static bool
lexerSingleQuoteOpensCharArray(char previousChar)
{
    const bool followsValue = (previousChar >= 'a' && previousChar <= 'z')
        || (previousChar >= 'A' && previousChar <= 'Z')
        || (previousChar >= '0' && previousChar <= '9') || previousChar == '_'
        || previousChar == ')' || previousChar == ']' || previousChar == '}'
        || previousChar == '\'' || previousChar == '.';
    return !followsValue;
}
//=============================================================================
static std::string
stripLineComment(const std::string& line)
{
    char stringDelimiter = '\0';
    size_t k = 0;
    while (k < line.size()) {
        const char ch = line[k];
        if (stringDelimiter != '\0') {
            if (ch == stringDelimiter) {
                if ((k + 1) < line.size() && line[k + 1] == stringDelimiter) {
                    k += 2; // escaped delimiter
                    continue;
                } else {
                    stringDelimiter = '\0';
                    k++;
                    continue;
                }
            }
            k++;
            continue;
        }
        if (ch == '"') {
            stringDelimiter = ch;
            k++;
            continue;
        }
        if (ch == '\'') {
            if (lexerSingleQuoteOpensCharArray(k > 0 ? line[k - 1] : '\0')) {
                stringDelimiter = ch;
            }
            k++;
            continue;
        }
        if (ch == '%') {
            return line.substr(0, k);
        }
        k++;
    }
    return line;
}
//=============================================================================
// Net change of ( [ { nesting over the code portion of a line (string literals
// and %/... comments excluded).
static int
lexerBracketDepthDelta(const std::string& line)
{
    char stringDelimiter = '\0';
    int depth = 0;
    for (size_t k = 0; k < line.size(); ++k) {
        const char ch = line[k];
        if (stringDelimiter != '\0') {
            if (ch == stringDelimiter) {
                if ((k + 1) < line.size() && line[k + 1] == stringDelimiter) {
                    ++k; // escaped delimiter
                    continue;
                }
                stringDelimiter = '\0';
            }
            continue;
        }
        if (ch == '"') {
            stringDelimiter = ch;
            continue;
        }
        if (ch == '\'') {
            if (lexerSingleQuoteOpensCharArray(k > 0 ? line[k - 1] : '\0')) {
                stringDelimiter = ch;
            }
            continue;
        }
        if (ch == '%') {
            break;
        }
        if (ch == '.' && k + 2 < line.size() && line[k + 1] == '.' && line[k + 2] == '.') {
            break;
        }
        if (ch == '(' || ch == '[' || ch == '{') {
            ++depth;
        } else if (ch == ')' || ch == ']' || ch == '}') {
            --depth;
        }
    }
    return depth;
}
//=============================================================================
// True when a line ends with an explicit "..." continuation (outside strings
// and before any "%" comment).
static bool
lexerLineHasExplicitContinuation(const std::string& line)
{
    const std::string code = stripLineComment(line);
    char stringDelimiter = '\0';
    size_t lastDots = std::string::npos;
    for (size_t k = 0; k < code.size(); ++k) {
        const char ch = code[k];
        if (stringDelimiter != '\0') {
            if (ch == stringDelimiter) {
                if ((k + 1) < code.size() && code[k + 1] == stringDelimiter) {
                    ++k;
                    continue;
                }
                stringDelimiter = '\0';
            }
            continue;
        }
        if (ch == '"') {
            stringDelimiter = ch;
            continue;
        }
        if (ch == '\'') {
            if (lexerSingleQuoteOpensCharArray(k > 0 ? code[k - 1] : '\0')) {
                stringDelimiter = ch;
            }
            continue;
        }
        if (ch == '.' && k + 2 < code.size() && code[k + 1] == '.' && code[k + 2] == '.') {
            lastDots = k;
            break;
        }
    }
    return lastDots != std::string::npos;
}
//=============================================================================
// Code portion of a line before an explicit "..." continuation (string-aware),
// or the comment-stripped line when there is none.
static std::string
withoutLexerLineContinuation(const std::string& line)
{
    const std::string code = stripLineComment(line);
    char stringDelimiter = '\0';
    for (size_t k = 0; k < code.size(); ++k) {
        const char ch = code[k];
        if (stringDelimiter != '\0') {
            if (ch == stringDelimiter) {
                if ((k + 1) < code.size() && code[k + 1] == stringDelimiter) {
                    ++k;
                    continue;
                }
                stringDelimiter = '\0';
            }
            continue;
        }
        if (ch == '"') {
            stringDelimiter = ch;
            continue;
        }
        if (ch == '\'') {
            if (lexerSingleQuoteOpensCharArray(k > 0 ? code[k - 1] : '\0')) {
                stringDelimiter = ch;
            }
            continue;
        }
        if (ch == '.' && k + 2 < code.size() && code[k + 1] == '.' && code[k + 2] == '.') {
            return code.substr(0, k);
        }
    }
    return code;
}
//=============================================================================
static bool
isFixedSizeDimension(const std::string& dimension)
{
    if (dimension.empty()) {
        return false;
    }
    for (const char ch : dimension) {
        if (std::isdigit(static_cast<unsigned char>(ch)) == 0) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static std::string
sizeValidationCode(const std::string& name, const std::string& sizeSpec)
{
    if (sizeSpec.empty()) {
        return {};
    }
    const std::string inner = trimLexerString(sizeSpec.substr(1, sizeSpec.size() - 2));
    std::vector<std::string> dims = splitTopLevelCommaList(inner);
    if (dims.empty()) {
        return {};
    }
    for (auto& dim : dims) {
        dim = trimLexerString(dim);
    }
    if (dims.size() == 2) {
        if (dims[0] == "1" && dims[1] == ":") {
            return "if size(" + name + ", 2) == 1 && size(" + name + ", 1) > 1; " + name + " = "
                + name + "'; end;\nmustBeRow(" + name + ");\n";
        }
        if (dims[0] == ":" && dims[1] == "1") {
            return "if size(" + name + ", 1) == 1 && size(" + name + ", 2) > 1; " + name + " = "
                + name + "'; end;\nmustBeColumn(" + name + ");\n";
        }
    }
    bool allFixed = true;
    bool anyFixed = false;
    for (const auto& dim : dims) {
        if (isFixedSizeDimension(dim)) {
            anyFixed = true;
        } else if (dim == ":") {
            allFixed = false;
        } else {
            // non literal dimension expression: leave unchecked
            return {};
        }
    }
    std::string generated;
    if (allFixed) {
        std::string dimsList;
        bool allOnes = true;
        for (size_t k = 0; k < dims.size(); ++k) {
            if (k != 0) {
                dimsList += ", ";
            }
            dimsList += dims[k];
            if (dims[k] != "1") {
                allOnes = false;
            }
        }
        // scalar expansion is a no-op when the declared size is all ones,
        // so the repmat branch is folded away at generation time
        if (!allOnes) {
            generated += "if isequal(size(" + name + "), [1, 1]); " + name + " = repmat(" + name
                + ", " + dimsList + "); end;\n";
        }
        if (allOnes && dims.size() == 2) {
            generated += "if ~isscalar(" + name
                + "); error(_('Value must match declared argument size.')); end;\n";
        } else {
            generated += "if ~isequal(size(" + name + ", 1:" + std::to_string(dims.size()) + "), ["
                + dimsList + "]) || ndims(" + name + ") > " + std::to_string(dims.size())
                + "; error(_('Value must match declared argument size.')); end;\n";
        }
        return generated;
    }
    if (!anyFixed) {
        // only colons: any size accepted, but the number of dimensions is bounded
        generated += "if ndims(" + name + ") > " + std::to_string(dims.size())
            + "; error(_('Value must match declared argument size.')); end;\n";
        return generated;
    }
    std::string condition;
    for (size_t k = 0; k < dims.size(); ++k) {
        if (!isFixedSizeDimension(dims[k])) {
            continue;
        }
        if (!condition.empty()) {
            condition += " && ";
        }
        condition += "size(" + name + ", " + std::to_string(k + 1) + ") == " + dims[k];
    }
    condition += " && ndims(" + name + ") <= " + std::to_string(dims.size());
    generated += "if ~(" + condition
        + "); error(_('Value must match declared argument size.')); end;\n";
    return generated;
}
//=============================================================================
static bool
parseDottedIdentifierName(const std::string& line, size_t& cursor, std::string& name)
{
    cursor = 0;
    name.clear();
    if (line.empty()
        || !(std::isalpha(static_cast<unsigned char>(line[cursor])) || line[cursor] == '_')) {
        return false;
    }
    cursor++;
    while (cursor < line.size() && isIdentifierLexerChar(line[cursor])) {
        cursor++;
    }
    while (cursor < line.size() && line[cursor] == '.') {
        const size_t dotPosition = cursor;
        cursor++;
        if (cursor >= line.size()
            || !(std::isalpha(static_cast<unsigned char>(line[cursor])) || line[cursor] == '_')) {
            cursor = dotPosition;
            break;
        }
        cursor++;
        while (cursor < line.size() && isIdentifierLexerChar(line[cursor])) {
            cursor++;
        }
    }
    name = line.substr(0, cursor);
    return true;
}
//=============================================================================
struct LexerArgumentDeclaration
{
    std::string name;
    std::string rootName;
    std::string fieldPath;
    std::string sizeSpec;
    std::string className;
    std::string validators;
    std::string defaultValue;
    bool hasDefault = false;
};
//=============================================================================
static bool
parseArgumentDeclarationParts(const std::string& sourceLine, LexerArgumentDeclaration& declaration)
{
    declaration = LexerArgumentDeclaration();
    std::string line = trimLexerString(stripLineComment(sourceLine));
    if (line.empty()) {
        return false;
    }

    size_t cursor = 0;
    std::string name;
    if (!parseDottedIdentifierName(line, cursor, name)) {
        return false;
    }
    std::string rest = trimLexerString(line.substr(cursor));

    const size_t defaultPos = findTopLevelChar(rest, '=');
    if (defaultPos != std::string::npos) {
        declaration.defaultValue = trimLexerString(rest.substr(defaultPos + 1));
        declaration.hasDefault = true;
        rest = trimLexerString(rest.substr(0, defaultPos));
    }

    if (!rest.empty() && rest[0] == '(') {
        int depth = 0;
        size_t end = std::string::npos;
        for (size_t k = 0; k < rest.size(); k++) {
            if (rest[k] == '(') {
                depth++;
            } else if (rest[k] == ')') {
                depth--;
                if (depth == 0) {
                    end = k;
                    break;
                }
            }
        }
        if (end != std::string::npos) {
            declaration.sizeSpec = rest.substr(0, end + 1);
            rest = trimLexerString(rest.substr(end + 1));
        }
    }

    const size_t validatorsStart = rest.find('{');
    if (validatorsStart != std::string::npos) {
        const size_t validatorsEnd = rest.rfind('}');
        if (validatorsEnd != std::string::npos && validatorsEnd > validatorsStart) {
            declaration.validators
                = rest.substr(validatorsStart + 1, validatorsEnd - validatorsStart - 1);
            rest = trimLexerString(rest.substr(0, validatorsStart));
        }
    }
    declaration.className = trimLexerString(rest);
    declaration.name = name;
    const size_t dotPos = name.find('.');
    if (dotPos == std::string::npos) {
        declaration.rootName = name;
    } else {
        declaration.rootName = name.substr(0, dotPos);
        declaration.fieldPath = name.substr(dotPos + 1);
    }
    return true;
}
//=============================================================================
static std::string
substituteIdentifierOccurrences(
    const std::string& text, const std::string& identifier, const std::string& replacement)
{
    if (identifier.empty() || identifier == replacement) {
        return text;
    }
    std::string result;
    size_t position = 0;
    while (position < text.size()) {
        const size_t found = text.find(identifier, position);
        if (found == std::string::npos) {
            result += text.substr(position);
            break;
        }
        const bool leftBoundary = found == 0
            || (!isIdentifierLexerChar(text[found - 1]) && text[found - 1] != '.');
        const size_t end = found + identifier.size();
        const bool rightBoundary = end >= text.size() || !isIdentifierLexerChar(text[end]);
        result += text.substr(position, found - position);
        if (leftBoundary && rightBoundary) {
            result += replacement;
        } else {
            result += identifier;
        }
        position = end;
    }
    return result;
}
//=============================================================================
static std::string
argumentValidationCode(const std::string& targetName, const LexerArgumentDeclaration& declaration)
{
    std::string generated;
    if (!declaration.className.empty()) {
        generated += "if ~isa(" + targetName + ", '" + declaration.className + "'); " + targetName
            + " = " + declaration.className + "(" + targetName + "); end;\n";
    }
    generated += sizeValidationCode(targetName, declaration.sizeSpec);
    for (const auto& validator : splitTopLevelCommaList(declaration.validators)) {
        if (validator.find('(') == std::string::npos) {
            generated += validator + "(" + targetName + ");\n";
        } else {
            generated
                += substituteIdentifierOccurrences(validator, declaration.name, targetName) + ";\n";
        }
    }
    return generated;
}
//=============================================================================
// Escape a resolved catalog message for a single-quoted generated literal.
static std::string
escapeForGeneratedLiteral(const std::string& text)
{
    std::string escaped;
    for (char c : text) {
        if (c == '\'') {
            escaped += "''";
        } else {
            escaped += c;
        }
    }
    return escaped;
}
static std::string
positionalArgumentCode(const LexerArgumentDeclaration& declaration, int position)
{
    std::string generated;
    if (!declaration.hasDefault) {
        generated += "if nargin < " + std::to_string(position)
            + "; error('Nelson:notEnoughInputs', '" + escapeForGeneratedLiteral(Nelson::resolveMessageTemplate("Nelson:notEnoughInputs", "Not enough input arguments.")) + "'); end;\n";
    } else {
        if (declaration.defaultValue.find("nargin") != std::string::npos) {
            generated += "if nargin < " + std::to_string(position)
                + "; error(_('Function nargin is not allowed in arguments block default "
                  "values.')); end;\n";
        } else {
            generated += "if nargin < " + std::to_string(position) + "; " + declaration.name + " = "
                + declaration.defaultValue + "; end;\n";
        }
    }
    generated += argumentValidationCode(declaration.name, declaration);
    return generated;
}
//=============================================================================
static std::string
rewriteOutputArgumentDeclaration(const std::string& sourceLine)
{
    std::string line = trimLexerString(stripLineComment(sourceLine));
    if (line.empty()) {
        return {};
    }

    size_t cursor = 0;
    std::string name;
    if (!parseDottedIdentifierName(line, cursor, name)) {
        return {};
    }
    std::string rest = trimLexerString(line.substr(cursor));

    std::string sizeSpec;
    if (!rest.empty() && rest[0] == '(') {
        int depth = 0;
        size_t end = std::string::npos;
        for (size_t k = 0; k < rest.size(); k++) {
            if (rest[k] == '(') {
                depth++;
            } else if (rest[k] == ')') {
                depth--;
                if (depth == 0) {
                    end = k;
                    break;
                }
            }
        }
        if (end != std::string::npos) {
            sizeSpec = rest.substr(0, end + 1);
            rest = trimLexerString(rest.substr(end + 1));
        }
    }

    std::string validators;
    const size_t validatorsStart = rest.find('{');
    if (validatorsStart != std::string::npos) {
        const size_t validatorsEnd = rest.rfind('}');
        if (validatorsEnd != std::string::npos && validatorsEnd > validatorsStart) {
            validators = rest.substr(validatorsStart + 1, validatorsEnd - validatorsStart - 1);
            rest = trimLexerString(rest.substr(0, validatorsStart));
        }
    }
    const std::string className = trimLexerString(rest);

    std::string generated;
    if (!className.empty()) {
        generated += "if ~isa(" + name + ", '" + className + "'); " + name + " = " + className + "("
            + name + "); end;\n";
    }
    generated += sizeValidationCode(name, sizeSpec);
    for (const auto& validator : splitTopLevelCommaList(validators)) {
        if (validator.find('(') == std::string::npos) {
            generated += validator + "(" + name + ");\n";
        } else {
            generated += validator + ";\n";
        }
    }
    if (generated.empty()) {
        return {};
    }
    return singleLineGeneratedCode("try;\n" + generated
        + "catch;\n__nelson_output_validation_error = lasterror();\nerror(['Invalid output ''"
        + name + "''. ', __nelson_output_validation_error.message]);\nend;\n");
}
//=============================================================================
static bool
isArgumentsBlockStart(const std::string& trimmed)
{
    return trimmed == "arguments" || startsWithLexerString(trimmed, "arguments ")
        || startsWithLexerString(trimmed, "arguments(");
}
//=============================================================================
enum class LexerArgumentsBlockKind
{
    Input,
    Output,
    Repeating,
    Invalid
};
//=============================================================================
static LexerArgumentsBlockKind
parseArgumentsBlockKind(const std::string& trimmed)
{
    std::string rest = trimLexerString(trimmed.substr(std::string("arguments").size()));
    if (rest.empty()) {
        return LexerArgumentsBlockKind::Input;
    }
    if (rest.front() != '(' || rest.back() != ')') {
        return LexerArgumentsBlockKind::Invalid;
    }
    rest = trimLexerString(rest.substr(1, rest.size() - 2));
    bool isOutput = false;
    bool isRepeating = false;
    for (const auto& token : splitTopLevelCommaList(rest)) {
        const std::string attribute = trimLexerString(token);
        if (attribute == "Input") {
            continue;
        }
        if (attribute == "Output") {
            isOutput = true;
        } else if (attribute == "Repeating") {
            isRepeating = true;
        } else {
            return LexerArgumentsBlockKind::Invalid;
        }
    }
    if (isOutput && isRepeating) {
        return LexerArgumentsBlockKind::Invalid;
    }
    if (isOutput) {
        return LexerArgumentsBlockKind::Output;
    }
    if (isRepeating) {
        return LexerArgumentsBlockKind::Repeating;
    }
    return LexerArgumentsBlockKind::Input;
}
//=============================================================================
static std::vector<std::string>
parseSignatureParameters(const std::string& signatureLine)
{
    std::vector<std::string> parameters;
    const size_t open = signatureLine.find('(');
    if (open == std::string::npos) {
        return parameters;
    }
    const size_t close = signatureLine.rfind(')');
    if (close == std::string::npos || close <= open) {
        return parameters;
    }
    for (const auto& token :
        splitTopLevelCommaList(signatureLine.substr(open + 1, close - open - 1))) {
        parameters.push_back(trimLexerString(token));
    }
    return parameters;
}
//=============================================================================
struct LexerFunctionArgumentsState
{
    std::vector<std::string> parameters;
    std::vector<LexerArgumentDeclaration> positionalEntries;
    std::vector<LexerArgumentDeclaration> repeatingEntries;
    std::vector<LexerArgumentDeclaration> nvEntries;
    std::string outputValidations;
    std::string invalidMessage;
    bool sawInputBlocks = false;
    bool sawOutputArgumentsBlock = false;
    bool flushed = false;
};
//=============================================================================
static std::string
isfieldChainCondition(const std::string& rootName, const std::string& fieldPath, bool positive)
{
    std::vector<std::string> segments;
    size_t start = 0;
    for (;;) {
        const size_t dot = fieldPath.find('.', start);
        if (dot == std::string::npos) {
            segments.push_back(fieldPath.substr(start));
            break;
        }
        segments.push_back(fieldPath.substr(start, dot - start));
        start = dot + 1;
    }
    std::string condition;
    std::string base = rootName;
    for (size_t k = 0; k < segments.size(); ++k) {
        if (!condition.empty()) {
            condition += positive ? " && " : " || ";
        }
        condition += positive ? "isfield(" + base + ", '" + segments[k] + "')"
                              : "~isfield(" + base + ", '" + segments[k] + "')";
        base += "." + segments[k];
    }
    return condition;
}
//=============================================================================
static std::string
buildArgumentsFlushCode(LexerFunctionArgumentsState& state, std::string& patchedSignature,
    const std::string& originalSignature)
{
    patchedSignature.clear();
    state.flushed = true;
    if (!state.invalidMessage.empty()) {
        return "error(_('" + state.invalidMessage + "'));";
    }
    std::string generated;
    const bool hasRepeating = !state.repeatingEntries.empty();
    const bool hasNameValue = !state.nvEntries.empty();
    const bool hasTail = hasRepeating || hasNameValue;

    std::string nvRootName;
    if (hasNameValue) {
        for (const auto& entry : state.nvEntries) {
            if (nvRootName.empty()) {
                nvRootName = entry.rootName;
            } else if (nvRootName != entry.rootName) {
                return "error(_('Only one name-value structure argument is supported.'));";
            }
        }
    }

    if (hasTail) {
        // The repeating and name-value parameters must be the trailing
        // parameters of the function signature; they are replaced by varargin.
        size_t tailStart = state.parameters.size();
        for (size_t k = 0; k < state.parameters.size(); ++k) {
            const std::string& parameter = state.parameters[k];
            bool isTailParameter = parameter == nvRootName;
            for (const auto& entry : state.repeatingEntries) {
                if (parameter == entry.name) {
                    isTailParameter = true;
                    break;
                }
            }
            if (isTailParameter) {
                tailStart = k;
                break;
            }
        }
        if (tailStart == state.parameters.size()
            && !(state.parameters.size() == 1 && state.parameters[0] == "varargin")) {
            return "error(_('arguments block does not match the function input arguments.'));";
        }
        if (tailStart != state.parameters.size()) {
            const size_t open = originalSignature.find('(');
            const size_t close = originalSignature.rfind(')');
            if (open == std::string::npos || close == std::string::npos || close <= open) {
                return "error(_('arguments block does not match the function input "
                       "arguments.'));";
            }
            std::string parameterList;
            for (size_t k = 0; k < tailStart; ++k) {
                if (k != 0) {
                    parameterList += ", ";
                }
                parameterList += state.parameters[k];
            }
            if (!parameterList.empty()) {
                parameterList += ", ";
            }
            parameterList += "varargin";
            patchedSignature = originalSignature.substr(0, open + 1) + parameterList
                + originalSignature.substr(close);
        }
        for (const auto& entry : state.repeatingEntries) {
            if (entry.hasDefault) {
                return "error(_('Repeating arguments cannot specify default values.'));";
            }
        }
    }

    int position = 1;
    for (const auto& entry : state.positionalEntries) {
        generated += positionalArgumentCode(entry, position);
        ++position;
    }

    if (hasTail) {
        generated += "__nls_tail = varargin;\n";
        if (hasNameValue) {
            std::string nvNamesList;
            std::vector<std::string> seenNames;
            for (const auto& entry : state.nvEntries) {
                std::string topName = entry.fieldPath;
                const size_t dot = topName.find('.');
                if (dot != std::string::npos) {
                    topName = topName.substr(0, dot);
                }
                bool known = false;
                for (const auto& seen : seenNames) {
                    if (seen == topName) {
                        known = true;
                        break;
                    }
                }
                if (known) {
                    continue;
                }
                seenNames.push_back(topName);
                if (!nvNamesList.empty()) {
                    nvNamesList += ", ";
                }
                nvNamesList += "'" + topName + "'";
            }
            generated += "__nls_nv_names = {" + nvNamesList + "};\n";
        }
        if (hasRepeating && hasNameValue) {
            generated += "__nls_nv_start = numel(__nls_tail) + 1;\n";
            generated += "for __nls_k = 1:numel(__nls_tail); __nls_v = __nls_tail{__nls_k}; "
                         "if ((ischar(__nls_v) && (size(__nls_v, 1) == 1)) || (isstring(__nls_v) "
                         "&& isscalar(__nls_v))) && any(strcmp(char(__nls_v), __nls_nv_names)); "
                         "__nls_nv_start = __nls_k; break; end; "
                         "if isstruct(__nls_v) && (__nls_k == numel(__nls_tail)); "
                         "__nls_nv_start = __nls_k; break; end; end;\n";
        } else if (hasRepeating) {
            generated += "__nls_nv_start = numel(__nls_tail) + 1;\n";
        } else {
            generated += "__nls_nv_start = 1;\n";
        }
        if (hasRepeating) {
            generated += "__nls_rep = __nls_tail(1:(__nls_nv_start - 1));\n";
            const std::string groupSize = std::to_string(state.repeatingEntries.size());
            generated += "if mod(numel(__nls_rep), " + groupSize
                + ") ~= 0; error(sprintf(_('Invalid argument list. Repeating arguments, if "
                  "specified, must be in groups of %d.'), "
                + groupSize + ")); end;\n";
            for (size_t k = 0; k < state.repeatingEntries.size(); ++k) {
                const LexerArgumentDeclaration& entry = state.repeatingEntries[k];
                generated += entry.name + " = __nls_rep(" + std::to_string(k + 1) + ":" + groupSize
                    + ":end);\n";
                const std::string validation = argumentValidationCode("__nls_e", entry);
                if (!validation.empty()) {
                    generated += "for __nls_k = 1:numel(" + entry.name + "); __nls_e = "
                        + entry.name + "{__nls_k};\n" + validation + entry.name
                        + "{__nls_k} = __nls_e; end;\n";
                }
            }
        }
        if (hasNameValue) {
            generated += "__nls_nvargs = __nls_tail(__nls_nv_start:end);\n";
            generated += nvRootName + " = struct();\n";
            generated += "if (numel(__nls_nvargs) == 1) && isstruct(__nls_nvargs{1}); "
                + nvRootName
                + " = __nls_nvargs{1}; elseif ~isempty(__nls_nvargs); "
                  "if mod(numel(__nls_nvargs), 2) ~= 0; "
                  "error('Nelson:interpreter:nameValuePair', '" + escapeForGeneratedLiteral(Nelson::resolveMessageTemplate("Nelson:interpreter:nameValuePair", "Name-value arguments must be paired.")) + "'); end; "
                  "for __nls_k = 1:2:numel(__nls_nvargs); __nls_n = __nls_nvargs{__nls_k}; "
                  "if isstring(__nls_n) && isscalar(__nls_n); __nls_n = char(__nls_n); end; "
                  "if ~ischar(__nls_n) || ~any(strcmp(__nls_n, __nls_nv_names)); "
                  "error(_('Unrecognized name-value argument.')); end; "
                + nvRootName + ".(__nls_n) = __nls_nvargs{__nls_k + 1}; end; end;\n";
            for (const auto& entry : state.nvEntries) {
                if (entry.hasDefault) {
                    if (entry.defaultValue.find("nargin") != std::string::npos) {
                        generated += "error(_('Function nargin is not allowed in arguments block "
                                     "default values.'));\n";
                        continue;
                    }
                    generated
                        += "if " + isfieldChainCondition(entry.rootName, entry.fieldPath, false)
                        + "; " + entry.name + " = " + entry.defaultValue + "; end;\n";
                }
                const std::string validation = argumentValidationCode(entry.name, entry);
                if (!validation.empty()) {
                    generated += "if "
                        + isfieldChainCondition(entry.rootName, entry.fieldPath, true) + ";\n"
                        + validation + "end;\n";
                }
            }
        }
    }
    return singleLineGeneratedCode(generated);
}
//=============================================================================
// Split a captured block into physical lines (trailing '\r' removed).
static std::vector<std::string>
splitBlockLines(const std::string& blockText)
{
    std::vector<std::string> lines;
    size_t start = 0;
    while (start <= blockText.size()) {
        size_t end = blockText.find('\n', start);
        std::string line;
        if (end == std::string::npos) {
            line = blockText.substr(start);
            start = blockText.size() + 1;
        } else {
            line = blockText.substr(start, end - start);
            start = end + 1;
        }
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        lines.push_back(line);
    }
    return lines;
}
//=============================================================================
// Process a single arguments block's body declarations into the accumulating
// function state. Mirrors the per-block loop of the historical rewrite: a
// logical declaration line is joined across implicit bracket continuations and
// explicit "..." continuations, and the block terminator "end" is never
// consumed.
static void
processBlockBody(const std::vector<std::string>& lines, LexerArgumentsBlockKind kind,
    LexerFunctionArgumentsState& state)
{
    size_t i = 1; // skip the header line
    while (i < lines.size()) {
        const std::string& line = lines[i];
        if (trimLexerString(stripLineComment(line)) == "end") {
            break;
        }
        bool explicitContinuation = lexerLineHasExplicitContinuation(line);
        std::string logicalLine = explicitContinuation
            ? trimLexerString(withoutLexerLineContinuation(line))
            : stripLineComment(line);
        int declarationDepth = lexerBracketDepthDelta(line);
        bool previousExplicit = explicitContinuation;
        ++i;
        while ((declarationDepth > 0 || previousExplicit) && i < lines.size()) {
            const std::string& nextLine = lines[i];
            if (trimLexerString(stripLineComment(nextLine)) == "end") {
                break;
            }
            ++i;
            const bool nextExplicit = lexerLineHasExplicitContinuation(nextLine);
            const std::string nextPiece = nextExplicit
                ? trimLexerString(withoutLexerLineContinuation(nextLine))
                : stripLineComment(nextLine);
            logicalLine += (declarationDepth > 0 && !previousExplicit) ? "\n" : " ";
            logicalLine += nextPiece;
            declarationDepth += lexerBracketDepthDelta(nextLine);
            previousExplicit = nextExplicit;
        }
        if (kind == LexerArgumentsBlockKind::Output) {
            const std::string validation = rewriteOutputArgumentDeclaration(logicalLine);
            if (!validation.empty()) {
                state.outputValidations += validation + " ";
            }
            continue;
        }
        LexerArgumentDeclaration declaration;
        if (parseArgumentDeclarationParts(logicalLine, declaration)) {
            state.sawInputBlocks = true;
            if (kind == LexerArgumentsBlockKind::Repeating) {
                if (!declaration.fieldPath.empty()) {
                    state.invalidMessage
                        = "Name-value arguments are not allowed in arguments (Repeating) "
                          "blocks.";
                } else {
                    state.repeatingEntries.push_back(declaration);
                }
            } else if (!declaration.fieldPath.empty()) {
                state.nvEntries.push_back(declaration);
            } else {
                state.positionalEntries.push_back(declaration);
            }
        }
    }
}
//=============================================================================
} // namespace
//=============================================================================
ArgumentsLoweringResult
lowerArgumentsBlocks(
    const std::vector<std::string>& blockTexts, const std::vector<std::string>& signatureParams)
{
    ArgumentsLoweringResult result;
    LexerFunctionArgumentsState state;
    state.parameters = signatureParams;
    // Inline errors are emitted at the header position of their block, so they
    // run before the (later) input flush code: preserve that source order.
    std::string inlineErrors;

    for (const std::string& blockText : blockTexts) {
        std::vector<std::string> lines = splitBlockLines(blockText);
        if (lines.empty()) {
            continue;
        }
        const std::string headerTrimmed = trimLexerString(stripLineComment(lines[0]));
        const LexerArgumentsBlockKind kind = parseArgumentsBlockKind(headerTrimmed);
        bool skip = false;
        if (kind == LexerArgumentsBlockKind::Invalid) {
            inlineErrors += "error(_('Invalid arguments block attribute.')); ";
            skip = true;
        } else if (kind != LexerArgumentsBlockKind::Output && state.sawOutputArgumentsBlock) {
            inlineErrors += "error(_('arguments (Input) block must appear before arguments (Output) "
                            "block.')); ";
            skip = true;
        }
        if (kind == LexerArgumentsBlockKind::Output) {
            state.sawOutputArgumentsBlock = true;
        }
        if (skip) {
            continue;
        }
        processBlockBody(lines, kind, state);
    }

    std::string flush;
    if (state.sawInputBlocks || !state.invalidMessage.empty()) {
        std::string patchedSignature;
        std::string originalSignature = "(";
        for (size_t k = 0; k < signatureParams.size(); ++k) {
            if (k != 0) {
                originalSignature += ", ";
            }
            originalSignature += signatureParams[k];
        }
        originalSignature += ")";
        flush = buildArgumentsFlushCode(state, patchedSignature, originalSignature);
        if (!patchedSignature.empty()) {
            result.patchedParams = parseSignatureParameters(patchedSignature);
            result.paramsPatched = true;
        }
    }

    // Prologue: inline structural errors followed by the input flush. These run
    // at the top of the body (call time), matching the historical rewrite where
    // inline errors sit on the block header lines and the input flush is emitted
    // at the first non-arguments statement.
    std::string prologue = inlineErrors;
    if (!flush.empty()) {
        if (!prologue.empty()) {
            prologue += " ";
        }
        prologue += flush;
    }
    result.generatedCode = trimLexerString(prologue);
    // Epilogue: output validations run at the end of the body, after the outputs
    // have been assigned (matching the historical rewrite, which merges them
    // just before the function 'end').
    result.outputValidationCode = trimLexerString(state.outputValidations);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
