//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RegularExpression.hpp"
#include <algorithm>
#include <cctype>
#include <memory>
#include <set>
#include <sstream>
#include <cwctype>
#include <climits>
#include <unordered_map>
#if defined(NELSON_USE_PCRE2)
#if WCHAR_MAX > 0xFFFF
#define PCRE2_CODE_UNIT_WIDTH 32
#else
#define PCRE2_CODE_UNIT_WIDTH 16
#endif
#include <pcre2.h>
#else
#include <pcre.h>
#endif
// Select the wide-character regex API based on platform:
//   Windows: bundled PCRE pcre16.
//   Linux/macOS: system PCRE2 pcre2-32.
#if defined(NELSON_USE_PCRE2)
#define NLS_PCRE_WIDE_TYPE pcre2_code
#define NLS_PCRE_WIDE_EXTRA_TYPE pcre2_match_data
#define NLS_PCRE_WIDE_SPTR PCRE2_SPTR
#define NLS_PCRE_WIDE_CHAR_TYPE PCRE2_UCHAR
#define nlsPcreWideFullinfo pcre2_pattern_info
#define nlsPcreWideFree pcre2_code_free
#define nlsPcreWideFreeStudy pcre2_match_data_free
#define NLS_PCRE_WIDE_UTF_FLAG PCRE2_UTF
#define NLS_PCRE_NO_WIDE_UTF_CHECK PCRE2_NO_UTF_CHECK
#define NLS_PCRE_DUPNAMES PCRE2_DUPNAMES
#define NLS_PCRE_CASELESS PCRE2_CASELESS
#define NLS_PCRE_DOTALL PCRE2_DOTALL
#define NLS_PCRE_MULTILINE PCRE2_MULTILINE
#define NLS_PCRE_INFO_CAPTURECOUNT PCRE2_INFO_CAPTURECOUNT
#define NLS_PCRE_INFO_NAMECOUNT PCRE2_INFO_NAMECOUNT
#define NLS_PCRE_INFO_NAMEENTRYSIZE PCRE2_INFO_NAMEENTRYSIZE
#define NLS_PCRE_INFO_NAMETABLE PCRE2_INFO_NAMETABLE
#define NLS_PCRE_UNSET PCRE2_UNSET
#else
#if WCHAR_MAX > 0xFFFF
#define NLS_PCRE_WIDE_TYPE pcre32
#define NLS_PCRE_WIDE_EXTRA_TYPE pcre32_extra
#define NLS_PCRE_WIDE_SPTR PCRE_SPTR32
#define NLS_PCRE_WIDE_CHAR_TYPE PCRE_UCHAR32
#define nlsPcreWideCompile2 pcre32_compile2
#define nlsPcreWideStudy pcre32_study
#define nlsPcreWideExec pcre32_exec
#define nlsPcreWideFullinfo pcre32_fullinfo
#define nlsPcreWideFree pcre32_free
#define nlsPcreWideFreeStudy pcre32_free_study
#define NLS_PCRE_WIDE_UTF_FLAG PCRE_UTF32
#define NLS_PCRE_NO_WIDE_UTF_CHECK PCRE_NO_UTF32_CHECK
#else
#define NLS_PCRE_WIDE_TYPE pcre16
#define NLS_PCRE_WIDE_EXTRA_TYPE pcre16_extra
#define NLS_PCRE_WIDE_SPTR PCRE_SPTR16
#define NLS_PCRE_WIDE_CHAR_TYPE PCRE_UCHAR16
#define nlsPcreWideCompile2 pcre16_compile2
#define nlsPcreWideStudy pcre16_study
#define nlsPcreWideExec pcre16_exec
#define nlsPcreWideFullinfo pcre16_fullinfo
#define nlsPcreWideFree pcre16_free
#define nlsPcreWideFreeStudy pcre16_free_study
#define NLS_PCRE_WIDE_UTF_FLAG PCRE_UTF16
#define NLS_PCRE_NO_WIDE_UTF_CHECK PCRE_NO_UTF16_CHECK
#endif
#define NLS_PCRE_DUPNAMES PCRE_DUPNAMES
#define NLS_PCRE_CASELESS PCRE_CASELESS
#define NLS_PCRE_DOTALL PCRE_DOTALL
#define NLS_PCRE_MULTILINE PCRE_MULTILINE
#define NLS_PCRE_INFO_CAPTURECOUNT PCRE_INFO_CAPTURECOUNT
#define NLS_PCRE_INFO_NAMECOUNT PCRE_INFO_NAMECOUNT
#define NLS_PCRE_INFO_NAMEENTRYSIZE PCRE_INFO_NAMEENTRYSIZE
#define NLS_PCRE_INFO_NAMETABLE PCRE_INFO_NAMETABLE
#define NLS_PCRE_UNSET -1
#endif
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "EvaluateCommand.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct NamedGroup
{
    std::string name;
    int number = 0;
};
//=============================================================================
struct RegexMatch
{
    int start = 0;
    int end = 0;
    std::wstring text;
    std::vector<int> tokenStart;
    std::vector<int> tokenEnd;
    std::vector<std::wstring> tokens;
    std::unordered_map<std::string, std::wstring> namedTokens;
};
//=============================================================================
struct RegexResult
{
    std::vector<RegexMatch> matches;
    std::vector<std::wstring> splits;
    std::vector<NamedGroup> namedGroups;
};
//=============================================================================
class PcreRegex;
//=============================================================================
static std::wstring
lower(const std::wstring& s);
static bool
isStringLikeScalar(const ArrayOf& a);
static bool
isTextContainer(const ArrayOf& a);
static ArrayOf
emptyChar();
static ArrayOf
cellFromVector(const ArrayOfVector& values, const Dimensions& dims);
static ArrayOf
cellRowFromStrings(const std::vector<std::wstring>& values);
static ArrayOf
cellRowFromArrays(const ArrayOfVector& values);
static ArrayOf
doubleRowFromInts(const std::vector<int>& values);
static ArrayOf
tokenExtentsMatrix(const RegexMatch& m);
static std::wstring
quoteNelsonString(const std::wstring& value);
static std::wstring
arrayToWideString(const ArrayOf& a);
static std::wstring
evaluateDynamicCommand(Evaluator* eval, const std::wstring& command);
static void
replaceAll(std::wstring& text, const std::wstring& from, const std::wstring& to);
static std::wstring
substituteTokensInCommand(std::wstring command, const RegexMatch* match);
static std::wstring
expandDynamicPattern(Evaluator* eval, const std::wstring& pattern);
static RegexOptions
parseOptions(const ArrayOfVector& args, size_t start, bool defaultIgnoreCase,
    std::vector<RegexOutputKind>& outputs, bool forReplace);
static std::vector<std::wstring>
getTextElements(const ArrayOf& a);
static Dimensions
getOutputDimensions(const ArrayOf& text, const ArrayOf& expr);
static std::wstring
elementAt(const std::vector<std::wstring>& values, size_t idx);
static ArrayOf
buildNamesStruct(const RegexResult& result);
static ArrayOf
buildOutput(const RegexResult& result, RegexOutputKind kind, const RegexOptions& options);
static ArrayOf
wrapIfForced(const ArrayOf& value, bool force);
ArrayOfVector
RegExpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool defaultIgnoreCase);
static bool
isAllLower(const std::wstring& s);
static bool
isAllUpper(const std::wstring& s);
static std::wstring
toCaseLike(const std::wstring& replacement, const std::wstring& original);
static std::wstring
replacementText(Evaluator* eval, const std::wstring& replacement, const RegexMatch& match,
    const std::wstring& input, const RegexOptions& options);
static std::wstring
regexReplace(Evaluator* eval, const std::wstring& input, const std::wstring& expr,
    const std::wstring& replacement, const RegexOptions& options);
ArrayOfVector
RegexPrepBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
static std::wstring
escapeRegex(const std::wstring& input);
static std::wstring
wildcardToRegex(const std::wstring& input);
ArrayOfVector
RegexpTranslateBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
class PcreRegex
{
private:
    NLS_PCRE_WIDE_TYPE* code = nullptr;
    NLS_PCRE_WIDE_EXTRA_TYPE* extra = nullptr;
    int captureCount = 0;
    std::vector<NamedGroup> namedGroups;

public:
    PcreRegex(const std::wstring& pattern, const RegexOptions& options, Evaluator* eval)
    {
        std::wstring expanded = expandDynamicPattern(eval, pattern);
        int compileOptions = NLS_PCRE_WIDE_UTF_FLAG | NLS_PCRE_DUPNAMES;
        if (options.ignoreCase) {
            compileOptions |= NLS_PCRE_CASELESS;
        }
        if (!options.dotExceptNewline) {
            compileOptions |= NLS_PCRE_DOTALL;
        }
        if (options.lineAnchors) {
            compileOptions |= NLS_PCRE_MULTILINE;
        }
#if defined(NELSON_USE_PCRE2)
        int errorCode = 0;
        PCRE2_SIZE errorOffset = 0;
        code = pcre2_compile(reinterpret_cast<NLS_PCRE_WIDE_SPTR>(expanded.c_str()),
            PCRE2_ZERO_TERMINATED, static_cast<uint32_t>(compileOptions), &errorCode, &errorOffset,
            nullptr);
        if (code == nullptr) {
            std::wstring message = _W("Invalid regular expression");
            PCRE2_UCHAR buffer[256] = { 0 };
            if (pcre2_get_error_message(errorCode, buffer, sizeof(buffer) / sizeof(buffer[0]))
                >= 0) {
                message += L": ";
                for (size_t i = 0; buffer[i] != 0; ++i) {
                    message.push_back(static_cast<wchar_t>(buffer[i]));
                }
            }
            Error(message);
        }
        pcre2_jit_compile(code, PCRE2_JIT_COMPLETE);
        extra = pcre2_match_data_create_from_pattern(code, nullptr);
        nlsPcreWideFullinfo(code, NLS_PCRE_INFO_CAPTURECOUNT, &captureCount);
#else
        const char* error = nullptr;
        int errorOffset = 0;
        code = nlsPcreWideCompile2(reinterpret_cast<NLS_PCRE_WIDE_SPTR>(expanded.c_str()),
            compileOptions, nullptr, &error, &errorOffset, nullptr);
        if (code == nullptr) {
            std::wstring message = _W("Invalid regular expression");
            if (error != nullptr) {
                message += L": " + utf8_to_wstring(error);
            }
            Error(message);
        }
        extra = nlsPcreWideStudy(code, PCRE_STUDY_JIT_COMPILE, &error);
        nlsPcreWideFullinfo(code, extra, NLS_PCRE_INFO_CAPTURECOUNT, &captureCount);
#endif
        int nameCount = 0;
        int entrySize = 0; // in NLS_PCRE_WIDE_CHAR_TYPE units
        NLS_PCRE_WIDE_CHAR_TYPE* nameTable = nullptr;
        nlsPcreWideFullinfo(code,
#if !defined(NELSON_USE_PCRE2)
            extra,
#endif
            NLS_PCRE_INFO_NAMECOUNT, &nameCount);
        nlsPcreWideFullinfo(code,
#if !defined(NELSON_USE_PCRE2)
            extra,
#endif
            NLS_PCRE_INFO_NAMEENTRYSIZE, &entrySize);
        nlsPcreWideFullinfo(code,
#if !defined(NELSON_USE_PCRE2)
            extra,
#endif
            NLS_PCRE_INFO_NAMETABLE, &nameTable);
        for (int i = 0; i < nameCount; ++i) {
            NLS_PCRE_WIDE_CHAR_TYPE* entry = nameTable + i * entrySize;
            int number = static_cast<int>(entry[0]);
            int nameOffset = 1;
            std::string name;
            for (int j = nameOffset; entry[j] != 0; ++j) {
                name.push_back(static_cast<char>(entry[j] & 0xFF));
            }
            namedGroups.push_back({ name, number });
        }
    }

    ~PcreRegex()
    {
        if (extra != nullptr) {
            nlsPcreWideFreeStudy(extra);
        }
        if (code != nullptr) {
            nlsPcreWideFree(code);
        }
    }

    const std::vector<NamedGroup>&
    names() const
    {
        return namedGroups;
    }

    RegexResult
    match(const std::wstring& wideText, const RegexOptions& options,
        const std::vector<RegexOutputKind>& outputs = {})
    {
        // Determine which fields to compute (skip unnecessary allocations)
        bool needsMatchText = outputs.empty();
        bool needsTokenData = outputs.empty();
        bool needsTokenText = outputs.empty();
        bool needsNamedTokens = outputs.empty();
        bool needsSplits = outputs.empty();
        for (auto k : outputs) {
            switch (k) {
            case RegexOutputKind::MATCH:
                needsMatchText = true;
                break;
            case RegexOutputKind::TOKENS:
                needsTokenData = true;
                needsTokenText = true;
                break;
            case RegexOutputKind::TOKEN_EXTENTS:
                needsTokenData = true;
                break;
            case RegexOutputKind::NAMES:
                needsNamedTokens = true;
                break;
            case RegexOutputKind::SPLIT:
                needsSplits = true;
                break;
            default:
                break;
            }
        }
        RegexResult result;
        result.namedGroups = namedGroups;
        // Pass the wstring directly to the wide PCRE API — no UTF-8 conversion needed.
        // Offsets returned in ovec are in wchar_t units (= character positions on all platforms).
        const int len = static_cast<int>(wideText.size());
        const NLS_PCRE_WIDE_SPTR subject = reinterpret_cast<NLS_PCRE_WIDE_SPTR>(wideText.c_str());
        const int ovecSize = std::max(3, (captureCount + 1) * 3);
#if defined(NELSON_USE_PCRE2)
        PCRE2_SIZE* ovec = nullptr;
#else
        std::vector<int> ovec(ovecSize);
#endif
        int startPos = 0;
        int previousEnd = 0;
        if (needsSplits) {
            result.splits.push_back(L"");
        }
        while (startPos <= len) {
            // NLS_PCRE_NO_WIDE_UTF_CHECK: skip per-call subject validation.
            // Input comes from a valid wstring so is always well-formed.
#if defined(NELSON_USE_PCRE2)
            int rc = pcre2_match(code, subject, static_cast<PCRE2_SIZE>(len),
                static_cast<PCRE2_SIZE>(startPos), NLS_PCRE_NO_WIDE_UTF_CHECK, extra, nullptr);
            if (rc >= 0) {
                ovec = pcre2_get_ovector_pointer(extra);
            }
#else
            int rc = nlsPcreWideExec(code, extra, subject, len, startPos,
                NLS_PCRE_NO_WIDE_UTF_CHECK, ovec.data(), static_cast<int>(ovec.size()));
#endif
            if (rc < 0) {
                break;
            }
            int matchStart = static_cast<int>(ovec[0]);
            int matchEnd = static_cast<int>(ovec[1]);
            if (matchStart == matchEnd && !options.emptyMatch) {
                if (++startPos > len) {
                    break;
                }
                continue;
            }
            RegexMatch m;
            // Offsets are wchar_t-unit indices — directly the Nelson 1-based/0-based positions.
            m.start = matchStart + 1;
            m.end = matchEnd;
            if (needsMatchText) {
                m.text = wideText.substr(
                    static_cast<size_t>(matchStart), static_cast<size_t>(matchEnd - matchStart));
            }
            int tokenCount = std::max(0, rc - 1);
            if (needsTokenData && tokenCount > 0) {
                m.tokenStart.reserve(tokenCount);
                m.tokenEnd.reserve(tokenCount);
                for (int i = 1; i <= tokenCount; ++i) {
                    if (ovec[i * 2] != NLS_PCRE_UNSET && ovec[i * 2 + 1] != NLS_PCRE_UNSET) {
                        m.tokenStart.push_back(static_cast<int>(ovec[i * 2]) + 1);
                        m.tokenEnd.push_back(static_cast<int>(ovec[i * 2 + 1]));
                        if (needsTokenText) {
                            m.tokens.push_back(wideText.substr(static_cast<size_t>(ovec[i * 2]),
                                static_cast<size_t>(ovec[i * 2 + 1] - ovec[i * 2])));
                        } else {
                            m.tokens.push_back(L"");
                        }
                    } else {
                        m.tokenStart.push_back(0);
                        m.tokenEnd.push_back(0);
                        m.tokens.push_back(L"");
                    }
                }
            }
            if (needsNamedTokens) {
                for (const auto& ng : namedGroups) {
                    int idx = ng.number;
                    if (idx <= tokenCount && ovec[idx * 2] != NLS_PCRE_UNSET
                        && m.namedTokens.find(ng.name) == m.namedTokens.end()) {
                        m.namedTokens[ng.name] = wideText.substr(static_cast<size_t>(ovec[idx * 2]),
                            static_cast<size_t>(ovec[idx * 2 + 1] - ovec[idx * 2]));
                    }
                }
            }
            if (needsSplits) {
                result.splits.back() = wideText.substr(static_cast<size_t>(previousEnd),
                    static_cast<size_t>(matchStart - previousEnd));
                result.splits.push_back(L"");
            }
            result.matches.push_back(std::move(m));
            previousEnd = matchEnd;
            if (options.once) {
                break;
            }
            startPos = matchEnd;
            if (matchStart == matchEnd) {
                ++startPos;
            }
        }
        if (needsSplits) {
            result.splits.back() = wideText.substr(static_cast<size_t>(previousEnd));
        }
        return result;
    }
};
//=============================================================================
ArrayOfVector
RegExpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool defaultIgnoreCase)
{
    nargincheck(argIn, 2);
    if (!isTextContainer(argIn[0]) || !isTextContainer(argIn[1])) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
    }
    std::vector<RegexOutputKind> outputs;
    RegexOptions options = parseOptions(argIn, 2, defaultIgnoreCase, outputs, false);
    if (argIn.size() == 2 && nLhs > 1 && outputs.size() == 1
        && outputs[0] == RegexOutputKind::START) {
        outputs.push_back(RegexOutputKind::END);
    }
    nargoutcheck(nLhs, 0, static_cast<int>(outputs.size()));
    if (nLhs == 0) {
        nLhs = 1;
    }
    std::vector<std::wstring> texts = getTextElements(argIn[0]);
    std::vector<std::wstring> exprs = getTextElements(argIn[1]);
    Dimensions dims = getOutputDimensions(argIn[0], argIn[1]);
    size_t elementCount = dims.getElementCount();
    // Match each element once and reuse results for all outputs.
    // Previously the inner loops re-compiled and re-matched for every output index.
    std::vector<RegexResult> allResults;
    allResults.reserve(elementCount);
    for (size_t k = 0; k < elementCount; ++k) {
        PcreRegex regex(elementAt(exprs, k), options, eval);
        allResults.push_back(regex.match(elementAt(texts, k), options, outputs));
    }
    ArrayOfVector retval;
    for (int outIdx = 0; outIdx < nLhs; ++outIdx) {
        ArrayOfVector cells;
        cells.reserve(elementCount);
        for (size_t k = 0; k < elementCount; ++k) {
            cells.push_back(buildOutput(allResults[k], outputs[outIdx], options));
        }
        bool scalarInput = elementCount == 1 && !options.forceCellOutput;
        if (scalarInput) {
            retval.push_back(cells[0]);
        } else {
            retval.push_back(cellFromVector(cells, dims));
        }
        if (options.forceCellOutput && elementCount == 1) {
            retval[outIdx] = wrapIfForced(cells[0], true);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
RegexPrepBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3);
    nargoutcheck(nLhs, 0, 1);
    if (!isTextContainer(argIn[0]) || !isTextContainer(argIn[1]) || !isTextContainer(argIn[2])) {
        Error(_W("Invalid input argument(s): cell or string expected."));
    }
    std::vector<RegexOutputKind> ignored;
    RegexOptions options = parseOptions(argIn, 3, false, ignored, true);
    std::vector<std::wstring> texts = getTextElements(argIn[0]);
    std::vector<std::wstring> exprs = getTextElements(argIn[1]);
    std::vector<std::wstring> repls = getTextElements(argIn[2]);
    Dimensions dims = getOutputDimensions(argIn[0], argIn[1]);
    if (repls.size() != 1 && repls.size() != dims.getElementCount()) {
        Error(_W("Same size expected."));
    }
    ArrayOfVector results;
    for (size_t k = 0; k < dims.getElementCount(); ++k) {
        results.push_back(ArrayOf::characterArrayConstructor(regexReplace(
            eval, elementAt(texts, k), elementAt(exprs, k), elementAt(repls, k), options)));
    }
    ArrayOfVector retval;
    if (dims.getElementCount() == 1 && isStringLikeScalar(argIn[0])) {
        ArrayOf value = results[0];
        if (argIn[0].isStringArray()) {
            value = ArrayOf::stringArrayConstructor(value.getContentAsWideString());
        }
        retval << value;
    } else {
        if (argIn[0].isStringArray()) {
            wstringVector v;
            for (auto& r : results) {
                v.push_back(r.getContentAsWideString());
            }
            retval << ArrayOf::stringArrayConstructor(v, dims);
        } else {
            retval << cellFromVector(results, dims);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
RegexpTranslateBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    if (!isStringLikeScalar(argIn[0]) || !isTextContainer(argIn[1])) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    std::wstring op = lower(argIn[0].getContentAsWideString());
    std::vector<std::wstring> values = getTextElements(argIn[1]);
    Dimensions dims = isStringLikeScalar(argIn[1]) ? Dimensions(1, 1) : argIn[1].getDimensions();
    std::wstring expression;
    if (op == L"flexible") {
        if (argIn.size() != 3 || !isStringLikeScalar(argIn[2])) {
            Error(_W("Third argument must be a string for 'flexible'."));
        }
        expression = argIn[2].getContentAsWideString();
    } else if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOfVector results;
    for (const auto& value : values) {
        if (op == L"escape") {
            results.push_back(ArrayOf::characterArrayConstructor(escapeRegex(value)));
        } else if (op == L"wildcard") {
            results.push_back(ArrayOf::characterArrayConstructor(wildcardToRegex(value)));
        } else if (op == L"flexible") {
            RegexOptions options;
            results.push_back(ArrayOf::characterArrayConstructor(
                regexReplace(nullptr, value, expression, escapeRegex(expression), options)));
        } else {
            Error(_W("'escape', 'wildcard' or 'flexible' expected."));
        }
    }
    ArrayOf out;
    if (isStringLikeScalar(argIn[1])) {
        out = results[0];
        if (argIn[1].isStringArray()) {
            out = ArrayOf::stringArrayConstructor(out.getContentAsWideString());
        }
    } else if (argIn[1].isStringArray()) {
        wstringVector v;
        for (auto& r : results) {
            v.push_back(r.getContentAsWideString());
        }
        out = ArrayOf::stringArrayConstructor(v, dims);
    } else {
        out = cellFromVector(results, dims);
    }
    ArrayOfVector retval;
    retval << out;
    return retval;
}
//=============================================================================
bool
isAllLower(const std::wstring& s)
{
    bool hasAlpha = false;
    for (wchar_t c : s) {
        if (std::iswalpha(c)) {
            hasAlpha = true;
            if (!std::iswlower(c)) {
                return false;
            }
        }
    }
    return hasAlpha;
}
//=============================================================================
bool
isAllUpper(const std::wstring& s)
{
    bool hasAlpha = false;
    for (wchar_t c : s) {
        if (std::iswalpha(c)) {
            hasAlpha = true;
            if (!std::iswupper(c)) {
                return false;
            }
        }
    }
    return hasAlpha;
}
//=============================================================================
std::wstring
toCaseLike(const std::wstring& replacement, const std::wstring& original)
{
    std::wstring res = replacement;
    if (isAllUpper(original)) {
        StringHelpers::to_upper(res);
    } else if (isAllLower(original)) {
        StringHelpers::to_lower(res);
    } else if (!original.empty() && std::iswupper(original[0]) && !res.empty()) {
        StringHelpers::to_lower(res);
        res[0] = static_cast<wchar_t>(std::towupper(res[0]));
    }
    return res;
}
//=============================================================================
std::wstring
replacementText(Evaluator* eval, const std::wstring& replacement, const RegexMatch& match,
    const std::wstring& input, const RegexOptions& options)
{
    std::wstring out;
    for (size_t i = 0; i < replacement.size();) {
        if (replacement.compare(i, 2, L"${") == 0) {
            size_t end = replacement.find(L'}', i + 2);
            if (end == std::wstring::npos) {
                out.append(replacement.substr(i));
                break;
            }
            std::wstring cmd
                = substituteTokensInCommand(replacement.substr(i + 2, end - i - 2), &match);
            out.append(evaluateDynamicCommand(eval, cmd));
            i = end + 1;
        } else if (replacement[i] == L'$') {
            if (i + 1 >= replacement.size()) {
                out.push_back(replacement[i++]);
            } else if (replacement[i + 1] == L'&' || replacement[i + 1] == L'0') {
                out.append(match.text);
                i += 2;
            } else if (replacement[i + 1] == L'`') {
                out.append(input.substr(0, match.start - 1));
                i += 2;
            } else if (replacement[i + 1] == L'\'') {
                out.append(input.substr(match.end));
                i += 2;
            } else if (replacement[i + 1] == L'<') {
                size_t end = replacement.find(L'>', i + 2);
                if (end != std::wstring::npos) {
                    std::string name = wstring_to_utf8(replacement.substr(i + 2, end - i - 2));
                    auto it = match.namedTokens.find(name);
                    if (it != match.namedTokens.end()) {
                        out.append(it->second);
                    }
                    i = end + 1;
                } else {
                    out.push_back(replacement[i++]);
                }
            } else if (std::iswdigit(replacement[i + 1])) {
                size_t j = i + 1;
                while (j < replacement.size() && std::iswdigit(replacement[j])) {
                    ++j;
                }
                int idx = std::stoi(replacement.substr(i + 1, j - i - 1));
                if (idx > 0 && idx <= static_cast<int>(match.tokens.size())) {
                    out.append(match.tokens[idx - 1]);
                }
                i = j;
            } else {
                out.push_back(replacement[i++]);
            }
        } else if (replacement[i] == L'\\' && i + 1 < replacement.size()) {
            wchar_t c = replacement[i + 1];
            if (c == L'n') {
                out.push_back(L'\n');
            } else if (c == L'r') {
                out.push_back(L'\r');
            } else if (c == L't') {
                out.push_back(L'\t');
            } else if (c == L'f') {
                out.push_back(L'\f');
            } else if (c == L'v') {
                out.push_back(L'\v');
            } else {
                out.push_back(c);
            }
            i += 2;
        } else {
            out.push_back(replacement[i++]);
        }
    }
    if (options.preserveCase) {
        out = toCaseLike(out, match.text);
    }
    return out;
}
//=============================================================================
std::wstring
regexReplace(Evaluator* eval, const std::wstring& input, const std::wstring& expr,
    const std::wstring& replacement, const RegexOptions& options)
{
    PcreRegex regex(expr, options, eval);
    RegexResult result = regex.match(input, options);
    if (result.matches.empty()) {
        return input;
    }
    std::wstring out;
    size_t last = 0;
    int occurrence = 0;
    for (const auto& m : result.matches) {
        ++occurrence;
        bool replaceThis
            = options.replaceOccurrence <= 0 || occurrence == options.replaceOccurrence;
        size_t start = static_cast<size_t>(m.start - 1);
        size_t end = static_cast<size_t>(m.end);
        out.append(input.substr(last, start - last));
        if (replaceThis) {
            out.append(replacementText(eval, replacement, m, input, options));
        } else {
            out.append(input.substr(start, end - start));
        }
        last = end;
        if (options.once && options.replaceOccurrence <= 0) {
            out.append(input.substr(last));
            return out;
        }
    }
    out.append(input.substr(last));
    return out;
}
//=============================================================================
std::wstring
escapeRegex(const std::wstring& input)
{
    const std::wstring special = L"\\.^$|()[]{}*+?";
    std::wstring out;
    for (wchar_t c : input) {
        if (special.find(c) != std::wstring::npos) {
            out.push_back(L'\\');
        }
        out.push_back(c);
    }
    return out;
}
//=============================================================================
std::wstring
wildcardToRegex(const std::wstring& input)
{
    std::wstring out;
    for (wchar_t c : input) {
        if (c == L'*') {
            out.append(L".*");
        } else if (c == L'?') {
            out.push_back(L'.');
        } else {
            out.append(escapeRegex(std::wstring(1, c)));
        }
    }
    return out;
}
//=============================================================================
std::wstring
lower(const std::wstring& s)
{
    return StringHelpers::to_lower_copy(s);
}
//=============================================================================
bool
isStringLikeScalar(const ArrayOf& a)
{
    return a.isRowVectorCharacterArray() || (a.isStringArray() && a.isScalar());
}
//=============================================================================
bool
isTextContainer(const ArrayOf& a)
{
    return isStringLikeScalar(a) || a.isCellArrayOfCharacterVectors() || a.isStringArray();
}
//=============================================================================
ArrayOf
emptyChar()
{
    Dimensions dims(1, 0);
    ArrayOf res = ArrayOf::emptyConstructor(dims);
    res.promoteType(NLS_CHAR);
    return res;
}
//=============================================================================
ArrayOf
cellFromVector(const ArrayOfVector& values, const Dimensions& dims)
{
    if (dims.getElementCount() == 0) {
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }
    ArrayOf* elements = nullptr;
    try {
        elements = new ArrayOf[dims.getElementCount()];
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
    }
    for (indexType k = 0; k < dims.getElementCount(); ++k) {
        elements[k] = values[k];
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
ArrayOf
cellRowFromStrings(const std::vector<std::wstring>& values)
{
    wstringVector v;
    v.reserve(values.size());
    for (const auto& s : values) {
        v.push_back(s);
    }
    return ArrayOf::toCellArrayOfCharacterRowVectors(v);
}
//=============================================================================
ArrayOf
cellRowFromArrays(const ArrayOfVector& values)
{
    Dimensions dims(1, values.size());
    return cellFromVector(values, dims);
}
//=============================================================================
ArrayOf
doubleRowFromInts(const std::vector<int>& values)
{
    if (values.empty()) {
        return ArrayOf::emptyConstructor();
    }
    std::vector<double> doubles;
    doubles.reserve(values.size());
    for (int v : values) {
        doubles.push_back(static_cast<double>(v));
    }
    return ArrayOf::doubleRowVectorConstructor(doubles);
}
//=============================================================================
ArrayOf
tokenExtentsMatrix(const RegexMatch& m)
{
    const indexType rows = static_cast<indexType>(m.tokenStart.size());
    if (rows == 0) {
        return ArrayOf::emptyConstructor(Dimensions(0, 2));
    }
    double* data = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, rows * 2, stringVector(), false));
    for (indexType r = 0; r < rows; ++r) {
        data[r] = static_cast<double>(m.tokenStart[r]);
        data[r + rows] = static_cast<double>(m.tokenEnd[r]);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(rows, 2), data);
}
//=============================================================================
std::wstring
quoteNelsonString(const std::wstring& value)
{
    std::wstring q = L"'";
    for (wchar_t c : value) {
        q.push_back(c);
        if (c == L'\'') {
            q.push_back(L'\'');
        }
    }
    q.push_back(L'\'');
    return q;
}
//=============================================================================
std::wstring
arrayToWideString(const ArrayOf& a)
{
    if (a.isRowVectorCharacterArray() || (a.isStringArray() && a.isScalar())) {
        return a.getContentAsWideString();
    }
    if (a.isNumeric() || a.isLogical()) {
        return a.getContentAsWideString();
    }
    return L"";
}
//=============================================================================
std::wstring
evaluateDynamicCommand(Evaluator* eval, const std::wstring& command)
{
    if (eval == nullptr) {
        Error(_W("Dynamic regular expressions require an evaluator."));
    }
    ArrayOfVector res = EvaluateCommand(eval, 1, command, L"");
    if (res.empty()) {
        return L"";
    }
    return arrayToWideString(res[0]);
}
//=============================================================================
void
replaceAll(std::wstring& text, const std::wstring& from, const std::wstring& to)
{
    if (from.empty()) {
        return;
    }
    size_t pos = 0;
    while ((pos = text.find(from, pos)) != std::wstring::npos) {
        text.replace(pos, from.size(), to);
        pos += to.size();
    }
}
//=============================================================================
std::wstring
substituteTokensInCommand(std::wstring command, const RegexMatch* match)
{
    if (match == nullptr) {
        return command;
    }
    replaceAll(command, L"$&", quoteNelsonString(match->text));
    replaceAll(command, L"$0", quoteNelsonString(match->text));
    for (size_t k = 0; k < match->tokens.size(); ++k) {
        replaceAll(command, L"$" + std::to_wstring(k + 1), quoteNelsonString(match->tokens[k]));
    }
    for (const auto& kv : match->namedTokens) {
        replaceAll(command, L"$<" + utf8_to_wstring(kv.first) + L">", quoteNelsonString(kv.second));
    }
    return command;
}
//=============================================================================
std::wstring
expandDynamicPattern(Evaluator* eval, const std::wstring& pattern)
{
    std::wstring out;
    for (size_t i = 0; i < pattern.size();) {
        if (pattern.compare(i, 4, L"(??@") == 0) {
            size_t end = pattern.find(L')', i + 4);
            if (end == std::wstring::npos) {
                out.append(pattern.substr(i));
                break;
            }
            out.append(evaluateDynamicCommand(eval, pattern.substr(i + 4, end - i - 4)));
            i = end + 1;
        } else if (pattern.compare(i, 3, L"(??") == 0) {
            size_t end = pattern.find(L')', i + 3);
            if (end == std::wstring::npos) {
                out.append(pattern.substr(i));
                break;
            }
            out.append(evaluateDynamicCommand(eval, pattern.substr(i + 3, end - i - 3)));
            i = end + 1;
        } else if (pattern.compare(i, 3, L"(?@") == 0) {
            size_t end = pattern.find(L')', i + 3);
            if (end == std::wstring::npos) {
                out.append(pattern.substr(i));
                break;
            }
            evaluateDynamicCommand(eval, pattern.substr(i + 3, end - i - 3));
            i = end + 1;
        } else {
            out.push_back(pattern[i++]);
        }
    }
    return out;
}
//=============================================================================
RegexOptions
parseOptions(const ArrayOfVector& args, size_t start, bool defaultIgnoreCase,
    std::vector<RegexOutputKind>& outputs, bool forReplace)
{
    RegexOptions options;
    options.ignoreCase = defaultIgnoreCase;
    for (size_t i = start; i < args.size(); ++i) {
        const ArrayOf& a = args[i];
        if (forReplace && a.isNumeric() && a.isScalar()) {
            double v = a.getContentAsDoubleScalar();
            options.replaceOccurrence = static_cast<int>(v);
            continue;
        }
        if (!isStringLikeScalar(a)) {
            Error(_W("Option must be a string."));
        }
        std::wstring opt = lower(a.getContentAsWideString());
        if (opt == L"start") {
            outputs.push_back(RegexOutputKind::START);
        } else if (opt == L"end") {
            outputs.push_back(RegexOutputKind::END);
        } else if (opt == L"tokenextents") {
            outputs.push_back(RegexOutputKind::TOKEN_EXTENTS);
        } else if (opt == L"match") {
            outputs.push_back(RegexOutputKind::MATCH);
        } else if (opt == L"tokens") {
            outputs.push_back(RegexOutputKind::TOKENS);
        } else if (opt == L"names") {
            outputs.push_back(RegexOutputKind::NAMES);
        } else if (opt == L"split") {
            outputs.push_back(RegexOutputKind::SPLIT);
        } else if (opt == L"once") {
            options.once = true;
        } else if (opt == L"all") {
            options.once = false;
        } else if (opt == L"forcecelloutput") {
            options.forceCellOutput = true;
        } else if (opt == L"ignorecase") {
            options.ignoreCase = true;
        } else if (opt == L"matchcase") {
            options.ignoreCase = false;
        } else if (opt == L"preservecase") {
            options.preserveCase = true;
            options.ignoreCase = true;
        } else if (opt == L"emptymatch") {
            options.emptyMatch = true;
        } else if (opt == L"noemptymatch") {
            options.emptyMatch = false;
        } else if (opt == L"dotexceptnewline") {
            options.dotExceptNewline = true;
        } else if (opt == L"dotall") {
            options.dotExceptNewline = false;
        } else if (opt == L"lineanchors") {
            options.lineAnchors = true;
        } else if (opt == L"stringanchors") {
            options.lineAnchors = false;
        } else if (opt == L"warnings") {
            options.warnings = true;
        } else if (opt == L"nowarnings") {
            options.warnings = false;
        } else {
            Error(_W("Unknown regular expression option."));
        }
    }
    if (outputs.empty() && !forReplace) {
        outputs.push_back(RegexOutputKind::START);
    }
    return options;
}
//=============================================================================
std::vector<std::wstring>
getTextElements(const ArrayOf& a)
{
    std::vector<std::wstring> values;
    if (isStringLikeScalar(a)) {
        values.push_back(a.getContentAsWideString());
    } else if (a.isCell() || a.isStringArray()) {
        indexType n = a.getElementCount();
        ArrayOf* elements = (ArrayOf*)a.getDataPointer();
        values.reserve(n);
        for (indexType k = 0; k < n; ++k) {
            if (elements[k].isRowVectorCharacterArray()) {
                values.push_back(elements[k].getContentAsWideString());
            } else {
                values.push_back(L"");
            }
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
    }
    return values;
}
//=============================================================================
Dimensions
getOutputDimensions(const ArrayOf& text, const ArrayOf& expr)
{
    if (isStringLikeScalar(text) && isStringLikeScalar(expr)) {
        return Dimensions(1, 1);
    }
    if (!isStringLikeScalar(text) && !isStringLikeScalar(expr)
        && !text.getDimensions().equals(expr.getDimensions())) {
        Error(_W("Same size expected."));
    }
    return isStringLikeScalar(text) ? expr.getDimensions() : text.getDimensions();
}
//=============================================================================
std::wstring
elementAt(const std::vector<std::wstring>& values, size_t idx)
{
    return values.size() == 1 ? values[0] : values[idx];
}
//=============================================================================
ArrayOf
buildNamesStruct(const RegexResult& result)
{
    stringVector names;
    for (const auto& ng : result.namedGroups) {
        if (std::find(names.begin(), names.end(), ng.name) == names.end()) {
            names.push_back(ng.name);
        }
    }
    if (names.empty()) {
        Dimensions dims(1, 0);
        return ArrayOf::emptyStructConstructor(names, dims);
    }
    if (result.matches.empty()) {
        Dimensions dims(1, 0);
        return ArrayOf::emptyStructConstructor(names, dims);
    }
    ArrayOfVector values;
    for (const auto& name : names) {
        ArrayOfVector cells;
        for (const auto& m : result.matches) {
            auto it = m.namedTokens.find(name);
            cells.push_back(
                ArrayOf::characterArrayConstructor(it == m.namedTokens.end() ? L"" : it->second));
        }
        values.push_back(cellRowFromArrays(cells));
    }
    return ArrayOf::structConstructor(names, values);
}
//=============================================================================
ArrayOf
buildOutput(const RegexResult& result, RegexOutputKind kind, const RegexOptions& options)
{
    switch (kind) {
    case RegexOutputKind::START: {
        std::vector<int> v;
        for (const auto& m : result.matches) {
            v.push_back(m.start);
        }
        return doubleRowFromInts(v);
    }
    case RegexOutputKind::END: {
        std::vector<int> v;
        for (const auto& m : result.matches) {
            v.push_back(m.end);
        }
        return doubleRowFromInts(v);
    }
    case RegexOutputKind::TOKEN_EXTENTS: {
        if (options.once) {
            if (result.matches.empty()) {
                return ArrayOf::emptyConstructor(Dimensions(0, 2));
            }
            return tokenExtentsMatrix(result.matches[0]);
        }
        ArrayOfVector cells;
        for (const auto& m : result.matches) {
            cells.push_back(tokenExtentsMatrix(m));
        }
        return cellRowFromArrays(cells);
    }
    case RegexOutputKind::MATCH: {
        if (options.once) {
            if (result.matches.empty()) {
                return emptyChar();
            }
            return ArrayOf::characterArrayConstructor(result.matches[0].text);
        }
        std::vector<std::wstring> v;
        for (const auto& m : result.matches) {
            v.push_back(m.text);
        }
        return cellRowFromStrings(v);
    }
    case RegexOutputKind::TOKENS: {
        if (options.once) {
            if (result.matches.empty()) {
                return cellRowFromStrings({});
            }
            return cellRowFromStrings(result.matches[0].tokens);
        }
        ArrayOfVector matchCells;
        for (const auto& m : result.matches) {
            matchCells.push_back(cellRowFromStrings(m.tokens));
        }
        return cellRowFromArrays(matchCells);
    }
    case RegexOutputKind::NAMES:
        return buildNamesStruct(result);
    case RegexOutputKind::SPLIT:
        return cellRowFromStrings(result.splits);
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
ArrayOf
wrapIfForced(const ArrayOf& value, bool force)
{
    if (!force) {
        return value;
    }
    ArrayOfVector v;
    v.push_back(value);
    return cellFromVector(v, Dimensions(1, 1));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
