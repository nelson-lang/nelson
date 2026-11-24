//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <cwchar>
#include <filesystem>
#include "GuiDialogs.hpp"
#include "GuiDialogProvider.hpp"
#include "GuiDialogsHelpers.hpp"
#include "DialogDirectories.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Builds a filter string ("Description (*.ext);;All Files (*)") from the
// uigetfile/uiputfile first argument. The web front end parses it.
static std::wstring
fileFilter(const ArrayOfVector& argIn)
{
    if (argIn.empty()) {
        return L"All Files (*)";
    }
    ArrayOf filter = argIn[0];
    if (filter.isCellArrayOfCharacterVectors()) {
        wstringVector parts = filter.asWideStringVector();
        if (parts.empty()) {
            return L"All Files (*)";
        }
        Dimensions dims = filter.dimensions();
        const size_t rows = (size_t)dims.rows();
        const size_t columns = (size_t)dims.columns();
        wstringVector filters;
        if (parts.size() == rows * columns && columns == 2 && rows >= 1) {
            // Nx2 cell: { pattern, description ; ... } (column major storage)
            for (size_t row = 0; row < rows; ++row) {
                const std::wstring pattern = parts[row];
                const std::wstring description = parts[rows + row];
                if (description.empty()) {
                    filters.push_back(pattern);
                } else {
                    filters.push_back(description + L" (" + pattern + L")");
                }
            }
        } else {
            for (const std::wstring& part : parts) {
                filters.push_back(part);
            }
        }
        std::wstring joined;
        for (const std::wstring& f : filters) {
            if (f.empty()) {
                continue;
            }
            if (!joined.empty()) {
                joined += L";;";
            }
            joined += f;
        }
        return joined.empty() ? L"All Files (*)" : joined;
    }
    if (filter.isRowVectorCharacterArray() || filter.isStringArray()) {
        return filter.asWideString();
    }
    return L"All Files (*)";
}
//=============================================================================
static std::wstring
rgbToHex(double r, double g, double b)
{
    auto clamp255 = [](double value) {
        long i = std::lround(value * 255.0);
        if (i < 0) {
            i = 0;
        }
        if (i > 255) {
            i = 255;
        }
        return (int)i;
    };
    wchar_t buffer[8];
    std::swprintf(buffer, 8, L"#%02x%02x%02x", clamp255(r), clamp255(g), clamp255(b));
    return buffer;
}
//=============================================================================
static bool
hexToRgb(const std::wstring& hex, double& r, double& g, double& b)
{
    std::wstring s = hex;
    if (!s.empty() && s[0] == L'#') {
        s = s.substr(1);
    }
    if (s.size() != 6) {
        return false;
    }
    try {
        int ri = std::stoi(s.substr(0, 2), nullptr, 16);
        int gi = std::stoi(s.substr(2, 2), nullptr, 16);
        int bi = std::stoi(s.substr(4, 2), nullptr, 16);
        r = ri / 255.0;
        g = gi / 255.0;
        b = bi / 255.0;
        return true;
    } catch (...) {
        return false;
    }
}
//=============================================================================
ArrayOfVector
UiGetFile(const ArrayOfVector& argIn, bool saveMode)
{
    std::wstring title = saveMode ? L"Save File" : L"Open File";
    if (argIn.size() > 1) {
        title = argIn[1].asWideString();
    }
    std::wstring explicitPath;
    if (argIn.size() > 2) {
        explicitPath = argIn[2].asWideString();
    }
    const std::wstring historyKey = saveMode ? L"desktop.save" : L"desktop.open";
    std::wstring explicitDirectory = explicitPath;
    std::wstring suggestedName;
    if (saveMode && !explicitPath.empty() && explicitPath != L".") {
        std::error_code ec;
        std::filesystem::path info(explicitPath);
        if (!std::filesystem::is_directory(info, ec)) {
            suggestedName = info.filename().wstring();
            std::wstring parent = info.parent_path().wstring();
            explicitDirectory = (parent == L".") ? std::wstring() : parent;
        }
    }
    std::error_code ec;
    const std::wstring currentPath = std::filesystem::current_path(ec).wstring();
    const std::wstring path = resolveDialogDirectory(historyKey, explicitDirectory, currentPath);

    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"file";
        request.title = title;
        request.fileMode = saveMode ? L"save" : L"open";
        request.defaultPath = path;
        request.suggestedName = suggestedName;
        request.historyKey = historyKey;
        request.fileFilters.push_back(fileFilter(argIn));
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            ArrayOfVector out;
            if (!response.ok || response.path.empty()) {
                out << ArrayOf::doubleScalar(0);
                out << ArrayOf::doubleScalar(0);
                out << ArrayOf::doubleScalar(0);
                return out;
            }
            std::filesystem::path picked(response.path);
            const std::wstring directory = picked.parent_path().wstring();
            setDialogDirectory(historyKey, directory);
            std::wstring directoryWithSeparator = directory;
            if (!directoryWithSeparator.empty() && directoryWithSeparator.back() != L'/'
                && directoryWithSeparator.back() != L'\\') {
                directoryWithSeparator += (wchar_t)std::filesystem::path::preferred_separator;
            }
            out << ArrayOf::characterArray(picked.filename().wstring());
            out << ArrayOf::characterArray(directoryWithSeparator);
            out << ArrayOf::doubleScalar(1);
            return out;
        }
    }
    Error(L"Nelson:uigetfile:noDisplay",
        L"File dialogs require a graphical desktop, which is not available.");
    ArrayOfVector out;
    out << ArrayOf::doubleScalar(0);
    out << ArrayOf::doubleScalar(0);
    out << ArrayOf::doubleScalar(0);
    return out;
}
//=============================================================================
ArrayOf
UiSetColor(const ArrayOfVector& argIn)
{
    double ir = 1.0;
    double ig = 1.0;
    double ib = 1.0;
    if (!argIn.empty() && argIn[0].isNumeric() && argIn[0].elementCount() >= 3) {
        ir = GuiDialogHelpers::numericElement(argIn[0], 0, 1);
        ig = GuiDialogHelpers::numericElement(argIn[0], 1, 1);
        ib = GuiDialogHelpers::numericElement(argIn[0], 2, 1);
    }
    std::wstring title;
    if (argIn.size() > 1) {
        title = argIn[1].asWideString();
    }
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"color";
        request.title = title.empty() ? _W("Select a color") : title;
        request.defaultPath = rgbToHex(ir, ig, ib);
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            if (!response.ok || response.path.empty()) {
                return ArrayOf::doubleScalar(0);
            }
            double r = 0.0;
            double g = 0.0;
            double b = 0.0;
            if (!hexToRgb(response.path, r, g, b)) {
                return ArrayOf::doubleScalar(0);
            }
            return ArrayOf::doubleRowVector({ r, g, b });
        }
    }
    Error(L"Nelson:uisetcolor:noDisplay",
        L"uisetcolor requires a graphical desktop, which is not available.");
    return ArrayOf::doubleScalar(0);
}
//=============================================================================
ArrayOf
UiSetFont(const ArrayOfVector& argIn)
{
    std::wstring family;
    double size = 0.0;
    if (!argIn.empty() && argIn[0].isStruct() && argIn[0].isScalar()) {
        if (argIn[0].isField("FontName")) {
            family = argIn[0].field("FontName").asWideString();
        }
        if (argIn[0].isField("FontSize")) {
            size = argIn[0].field("FontSize").asDoubleScalar();
        }
    }
    std::wstring title;
    if (argIn.size() > 1) {
        title = argIn[1].asWideString();
    }
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"font";
        request.title = title.empty() ? _W("Select a font") : title;
        request.fontFamily = family;
        request.fontSize = size > 0 ? size : 10.0;
        request.fontBold = false;
        request.fontItalic = false;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            if (!response.ok || response.values.size() < 2) {
                return ArrayOf::doubleScalar(0);
            }
            double sz = 10.0;
            try {
                sz = std::stod(response.values[1]);
            } catch (...) {
            }
            ArrayOfVector fields;
            fields << ArrayOf::characterArray(response.values[0]);
            fields << ArrayOf::doubleScalar(sz);
            fields << ArrayOf::characterArray(
                response.values.size() > 2 ? response.values[2] : L"normal");
            fields << ArrayOf::characterArray(
                response.values.size() > 3 ? response.values[3] : L"normal");
            fields << ArrayOf::characterArray(L"points");
            return ArrayOf::structScalar(
                { "FontName", "FontSize", "FontWeight", "FontAngle", "FontUnits" }, fields);
        }
    }
    Error(L"Nelson:uisetfont:noDisplay",
        L"uisetfont requires a graphical desktop, which is not available.");
    return ArrayOf::doubleScalar(0);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
