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
#include "DisplayVariable.hpp"
#include "DisplayVariableHelpers.hpp"
#include "ComputeFormatInfo.hpp"
#include "DisplayCell.hpp"
#include "DisplayStruct.hpp"
#include "DisplayClass.hpp"
#include "DisplayString.hpp"
#include "DisplayChar.hpp"
#include "DisplayLogical.hpp"
#include "DisplayInteger.hpp"
#include "DisplayDouble.hpp"
#include "DisplayDoubleComplex.hpp"
#include "DisplaySingle.hpp"
#include "DisplaySingleComplex.hpp"
#include "DisplaySparseDouble.hpp"
#include "DisplaySparseDoubleComplex.hpp"
#include "DisplayMissing.hpp"
#include "FormatHelpers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <limits>
#include <vector>
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class BufferedDisplayInterface : public Interface
    {
    public:
        BufferedDisplayInterface(size_t width, size_t height) : termWidth(width), termHeight(height)
        {
        }

        std::string
        getLine(const std::string& prompt) override
        {
            (void)prompt;
            return "";
        }

        std::wstring
        getLine(const std::wstring& prompt) override
        {
            (void)prompt;
            return L"";
        }

        std::wstring
        getInput(const std::wstring& prompt) override
        {
            (void)prompt;
            return L"";
        }

        size_t
        getTerminalWidth() override
        {
            return termWidth;
        }

        size_t
        getTerminalHeight() override
        {
            return termHeight;
        }

        void
        outputMessage(const std::string& msg) override
        {
            output.append(utf8_to_wstring(msg));
        }

        void
        outputMessage(const std::wstring& msg) override
        {
            output.append(msg);
        }

        void
        errorMessage(const std::string& msg) override
        {
            outputMessage(msg);
        }

        void
        errorMessage(const std::wstring& msg) override
        {
            outputMessage(msg);
        }

        void
        warningMessage(const std::string& msg) override
        {
            outputMessage(msg);
        }

        void
        warningMessage(const std::wstring& msg) override
        {
            outputMessage(msg);
        }

        void
        clearTerminal() override
        {
            output.clear();
        }

        bool
        isAtPrompt() override
        {
            return false;
        }

        void
        interruptGetLineByEvent() override
        {
        }

        std::wstring output;

    private:
        size_t termWidth;
        size_t termHeight;
    };
    //=============================================================================
    bool
    isTruncatableMatrix(const ArrayOf& A)
    {
        if (!A.is2D() || A.isScalar() || A.isEmpty()) {
            return false;
        }
        switch (A.dataClass()) {
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_LOGICAL:
        case NLS_DOUBLE:
        case NLS_DCOMPLEX:
        case NLS_SINGLE:
        case NLS_SCOMPLEX: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    }
    //=============================================================================
    bool
    isIntegerMatrix(const ArrayOf& A)
    {
        switch (A.dataClass()) {
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64: {
            return true;
        } break;
        default: {
            return false;
        } break;
        }
    }
    //=============================================================================
    size_t
    nominalMatrixColumnWidth(const ArrayOf& A)
    {
        auto config = NelsonConfiguration::getInstance();
        NumericFormatDisplay numericFormat = config->getNumericFormatDisplay();
        if (numericFormat == NLS_NUMERIC_FORMAT_PLUS) {
            return 1;
        }

        if (A.dataClass() == NLS_LOGICAL) {
            return std::wstring(BLANKS_AT_BOL).length() + std::wstring(L"false").length();
        }

        if (isIntegerMatrix(A)) {
            return std::wstring(L"   ").length() + 24;
        }

        FormatDisplayInformation formatInfo = computeFormatInfo(A, numericFormat);
        return std::max<size_t>(1, static_cast<size_t>(getNominalWidth(formatInfo)));
    }
    //=============================================================================
    bool
    matrixColumnsOverflow(const ArrayOf& A, size_t width)
    {
        if (A.isSparse()) {
            return false;
        }
        Dimensions dims = A.dimensions();
        size_t nominalColumnWidth = nominalMatrixColumnWidth(A);
        return nominalColumnWidth > 0
            && static_cast<size_t>(dims.columns()) > (width / nominalColumnWidth);
    }
    //=============================================================================
    size_t
    previewTerminalWidth(const ArrayOf& A, size_t width)
    {
        if (A.isSparse()) {
            return width;
        }
        Dimensions dims = A.dimensions();
        size_t columns = static_cast<size_t>(dims.columns());
        size_t nominalColumnWidth = nominalMatrixColumnWidth(A);
        if (nominalColumnWidth == 0
            || columns > (std::numeric_limits<size_t>::max() - 1) / nominalColumnWidth) {
            return width;
        }
        return std::max(width, columns * nominalColumnWidth + 1);
    }
    //=============================================================================
    size_t
    truncatedPreviewWidth(const ArrayOf& A, size_t width, bool columnsOverflow)
    {
        if (!columnsOverflow || A.isSparse()) {
            return width;
        }
        size_t nominalColumnWidth = nominalMatrixColumnWidth(A);
        if (width <= nominalColumnWidth + 1) {
            return width;
        }
        return width - nominalColumnWidth;
    }
    //=============================================================================
    bool
    mayNeedMatrixTruncation(const ArrayOf& A, size_t width, size_t height)
    {
        size_t visibleHeight = height > 3 ? height - 3 : 1;
        if (A.isSparse()) {
            return static_cast<size_t>(A.nonzeros()) > visibleHeight;
        }

        Dimensions dims = A.dimensions();
        if (static_cast<size_t>(dims.rows()) > visibleHeight) {
            return true;
        }

        return matrixColumnsOverflow(A, width);
    }
    //=============================================================================
    std::vector<std::wstring>
    splitLines(const std::wstring& text)
    {
        std::vector<std::wstring> lines;
        std::wstring currentLine;
        for (wchar_t c : text) {
            if (c == L'\n') {
                lines.push_back(currentLine);
                currentLine.clear();
            } else if (c != L'\r') {
                currentLine.push_back(c);
            }
        }
        if (!currentLine.empty() || text.empty() || text.back() != L'\n') {
            lines.push_back(currentLine);
        }
        return lines;
    }
    //=============================================================================
    std::wstring
    matrixSizeText(const ArrayOf& A)
    {
        Dimensions dims = A.dimensions();
        return fmt::format(
            L"{}x{}", static_cast<long long>(dims.rows()), static_cast<long long>(dims.columns()));
    }
    //=============================================================================
    std::wstring
    trimCopy(const std::wstring& text)
    {
        size_t first = text.find_first_not_of(L" \t");
        if (first == std::wstring::npos) {
            return L"";
        }
        size_t last = text.find_last_not_of(L" \t");
        return text.substr(first, last - first + 1);
    }
    //=============================================================================
    bool
    hasHeaderDimensionPrefix(const std::wstring& trimmed, const std::wstring& dimensionText)
    {
        return !dimensionText.empty()
            && (trimmed == dimensionText || trimmed.rfind(dimensionText + L" ", 0) == 0);
    }
    //=============================================================================
    bool
    isStandaloneMatrixHeaderLine(const std::wstring& line, const ArrayOf& A)
    {
        std::wstring trimmed = trimCopy(line);
        return hasHeaderDimensionPrefix(trimmed, matrixSizeText(A))
            || hasHeaderDimensionPrefix(trimmed, A.dimensions().toWideString());
    }
    //=============================================================================
    void
    moveMatrixSizeToNameLine(std::vector<std::wstring>& lines, const ArrayOf& A)
    {
        std::wstring sizeText = matrixSizeText(A);
        for (size_t k = 0; k < lines.size(); ++k) {
            std::wstring trimmed = trimCopy(lines[k]);
            if (trimmed.length() < 2 || trimmed.back() != L'=') {
                continue;
            }

            size_t next = k + 1;
            while (next < lines.size() && trimCopy(lines[next]).empty()) {
                lines.erase(lines.begin() + next);
            }

            std::wstring headerText = sizeText;
            if (next < lines.size() && isStandaloneMatrixHeaderLine(lines[next], A)) {
                headerText = trimCopy(lines[next]);
                lines.erase(lines.begin() + next);
            }
            lines[k] = lines[k] + L" " + headerText;

            while (next < lines.size() && trimCopy(lines[next]).empty()) {
                lines.erase(lines.begin() + next);
            }
            return;
        }
    }
    //=============================================================================
    std::wstring
    truncateLineToWidth(const std::wstring& line, size_t width)
    {
        if (width == 0 || line.length() <= width) {
            return line;
        }
        static const std::wstring ellipsis = L" " HORIZONTAL_ELLIPSIS;
        if (width <= ellipsis.length()) {
            return ellipsis.substr(0, width);
        }
        size_t cutPosition = width - ellipsis.length();
        size_t separatorPosition = cutPosition;
        while (separatorPosition > 0 && line[separatorPosition - 1] != L' '
            && line[separatorPosition - 1] != L'\t') {
            --separatorPosition;
        }
        if (separatorPosition > std::wstring(BLANKS_AT_BOL).length()) {
            cutPosition = separatorPosition;
            while (cutPosition > 0
                && (line[cutPosition - 1] == L' ' || line[cutPosition - 1] == L'\t')) {
                --cutPosition;
            }
        }
        return line.substr(0, cutPosition) + ellipsis;
    }
    //=============================================================================
    bool
    needsTruncation(const std::vector<std::wstring>& lines, size_t width, size_t height)
    {
        if (lines.size() > height) {
            return true;
        }
        for (const std::wstring& line : lines) {
            if (line.length() > width) {
                return true;
            }
        }
        return false;
    }
    //=============================================================================
    std::wstring
    buildTruncatedText(const std::vector<std::wstring>& lines, size_t width, size_t height)
    {
        bool lineOverflow = lines.size() > height;
        size_t visibleLineCount = lineOverflow ? std::max<size_t>(1, height / 2) : height;
        visibleLineCount = std::max<size_t>(1, std::min(visibleLineCount, lines.size()));

        std::wstring truncatedText;
        for (size_t k = 0; k < visibleLineCount; ++k) {
            truncatedText.append(truncateLineToWidth(lines[k], width));
            truncatedText.push_back(L'\n');
        }
        if (visibleLineCount < lines.size()) {
            truncatedText.append(BLANKS_AT_BOL);
            truncatedText.append(L"\u22EE\n");
        }
        return truncatedText;
    }
    //=============================================================================
    bool
    tryDisplayTruncatedMatrix(size_t evaluatorID, Interface* io, const ArrayOf& A,
        const std::string& name, bool asDisp, bool& needToOverload)
    {
        auto config = NelsonConfiguration::getInstance();
        if (!io || !io->supportsTruncatedMatrixDisplay() || !config->getTruncateMatricesDisplay()
            || !isTruncatableMatrix(A)) {
            return false;
        }

        size_t width = std::max<size_t>(1, io->getTerminalWidth());
        size_t height = std::max<size_t>(1, io->getTerminalHeight());
        if (!mayNeedMatrixTruncation(A, width, height)) {
            return false;
        }

        bool columnsOverflow = matrixColumnsOverflow(A, width);
        BufferedDisplayInterface capture(width, height);
        bool capturedNeedToOverload = false;
        DisplayVariable(evaluatorID, &capture, A, name, asDisp, capturedNeedToOverload);
        if (capturedNeedToOverload) {
            needToOverload = true;
            return true;
        }

        std::vector<std::wstring> lines = splitLines(capture.output);
        moveMatrixSizeToNameLine(lines, A);
        if (!columnsOverflow && !needsTruncation(lines, width, height)) {
            io->outputMessage(capture.output);
            needToOverload = false;
            return true;
        }

        if (columnsOverflow) {
            BufferedDisplayInterface previewCapture(previewTerminalWidth(A, width), height);
            bool previewNeedToOverload = false;
            DisplayVariable(evaluatorID, &previewCapture, A, name, asDisp, previewNeedToOverload);
            if (previewNeedToOverload) {
                needToOverload = true;
                return true;
            }
            lines = splitLines(previewCapture.output);
            moveMatrixSizeToNameLine(lines, A);
        }

        Dimensions dims = A.dimensions();
        std::wstring label = formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:display_format:displayAllRowsColumns",
                L"Display all %d rows and %d columns"),
            static_cast<long long>(dims.rows()), static_cast<long long>(dims.columns()));
        size_t previewWidth = truncatedPreviewWidth(A, width, columnsOverflow);
        std::wstring truncatedText = buildTruncatedText(lines, previewWidth, height);
        bool displayed = io->outputTruncatedMatrix(
            truncatedText, label, capture.output, utf8_to_wstring(name));
        if (!displayed) {
            io->outputMessage(capture.output);
        }
        needToOverload = false;
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
DisplayVariable(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::string& name,
    bool asDisp, bool& needToOverload)
{
    if (io == nullptr) {
        return;
    }
    if (tryDisplayTruncatedMatrix(evaluatorID, io, A, name, asDisp, needToOverload)) {
        return;
    }
    std::wstring wname = utf8_to_wstring(name);
    switch (A.dataClass()) {
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        DisplayInteger(evaluatorID, io, A, wname, asDisp);
        needToOverload = false;
    } break;
    case NLS_LOGICAL: {
        DisplayLogical(evaluatorID, io, A, wname, asDisp);
        needToOverload = false;
    } break;
    case NLS_CHAR: {
        DisplayChar(evaluatorID, io, A, wname, asDisp);
        needToOverload = false;
    } break;
    case NLS_STRING_ARRAY: {
        DisplayString(evaluatorID, io, A, wname, asDisp);
        needToOverload = false;
    } break;
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY: {
        bool needToHideHeader = false;
        if (asDisp) {
            needToHideHeader = (A.isEmpty() || A.isScalar());
        } else {
            needToHideHeader = false;
        }
        DisplayClass(io, A, wname, needToHideHeader);
        needToOverload = false;
    } break;
    case NLS_FUNCTION_HANDLE: {
        needToOverload = true;
    } break;
    case NLS_STRUCT_ARRAY: {
        bool needToHideHeader = false;
        if (asDisp) {
            needToHideHeader = (A.isEmpty() || A.isScalar());
        } else {
            needToHideHeader = false;
        }
        DisplayStruct(io, A, wname, needToHideHeader);
        needToOverload = false;
    } break;
    case NLS_CELL_ARRAY: {
        DisplayCell(evaluatorID, io, A, wname, asDisp);
        needToOverload = false;
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            DisplaySparseDouble(evaluatorID, io, A, wname, asDisp);
        } else {
            DisplayDouble(evaluatorID, io, A, wname, asDisp);
        }
        needToOverload = false;
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            DisplaySparseDoubleComplex(evaluatorID, io, A, wname, asDisp);
        } else {
            DisplayDoubleComplex(evaluatorID, io, A, wname, asDisp);
        }
        needToOverload = false;
    } break;
    case NLS_SINGLE: {
        if (A.isSparse()) {
            DisplaySparseDouble(evaluatorID, io, A, wname, asDisp);
        } else {
            DisplaySingle(evaluatorID, io, A, wname, asDisp);
        }
        needToOverload = false;
    } break;
    case NLS_SCOMPLEX: {
        if (A.isSparse()) {
            DisplaySparseDoubleComplex(evaluatorID, io, A, wname, asDisp);
        } else {
            DisplaySingleComplex(evaluatorID, io, A, wname, asDisp);
        }
        needToOverload = false;
    } break;
    case NLS_MISSING_ARRAY: {
        DisplayMissing(evaluatorID, io, A, wname, asDisp);
        needToOverload = false;
    } break;

    default: {
        needToOverload = true;
    } break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
