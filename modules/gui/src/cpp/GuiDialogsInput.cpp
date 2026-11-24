//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GuiDialogs.hpp"
#include "GuiDialogProvider.hpp"
#include "GuiDialogsHelpers.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
InputDialog(const ArrayOfVector& argIn)
{
    wstringVector prompts = GuiDialogHelpers::asTextList(argIn[0]);
    std::wstring title = GuiDialogHelpers::argText(argIn, 1, _W("Input Dialog"));
    // Third argument: number of lines of each edit field (scalar or per prompt).
    std::vector<double> numLines;
    if (argIn.size() > 2 && argIn[2].isNumeric() && !argIn[2].isEmpty()) {
        numLines = argIn[2].asDoubleVector();
    }
    wstringVector defaults;
    if (argIn.size() > 3) {
        defaults = GuiDialogHelpers::asTextList(argIn[3]);
    }

    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"input";
        request.title = title;
        for (size_t k = 0; k < prompts.size(); ++k) {
            GuiDialogField field;
            field.prompt = prompts[k];
            field.value = defaults.size() > k ? defaults[k] : std::wstring();
            field.lines = 1;
            if (!numLines.empty()) {
                double requested = numLines.size() > k ? numLines[k] : numLines[0];
                if (requested >= 1.0 && requested < 100.0) {
                    field.lines = static_cast<int>(requested);
                }
            }
            request.fields.push_back(field);
        }
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            if (!response.ok) {
                return ArrayOf::emptyCell(Dimensions(0, 0));
            }
            ArrayOfMatrix matrix;
            for (size_t k = 0; k < prompts.size(); ++k) {
                ArrayOfVector row;
                row << ArrayOf::characterArray(
                    response.values.size() > k ? response.values[k] : std::wstring());
                matrix.push_back(row);
            }
            return ArrayOf::cellArray(matrix);
        }
    }
    Error(L"Nelson:inputdlg:noDisplay",
        L"inputdlg requires a graphical desktop, which is not available.");
    return ArrayOf::emptyCell(Dimensions(0, 0));
}
//=============================================================================
ArrayOfVector
ListDialog(const ArrayOfVector& argIn)
{
    wstringVector items;
    std::wstring title = _W("List Dialog");
    std::wstring prompt;
    bool multiple = false;
    std::vector<double> initial;
    for (size_t k = 0; k + 1 < argIn.size(); k += 2) {
        if (GuiDialogHelpers::isName(argIn[k], L"ListString")) {
            items = GuiDialogHelpers::asTextList(argIn[k + 1]);
        } else if (GuiDialogHelpers::isName(argIn[k], L"Name")) {
            title = argIn[k + 1].asWideString();
        } else if (GuiDialogHelpers::isName(argIn[k], L"PromptString")) {
            prompt = GuiDialogHelpers::asText(argIn[k + 1]);
        } else if (GuiDialogHelpers::isName(argIn[k], L"SelectionMode")) {
            multiple = StringHelpers::iequals(argIn[k + 1].asWideString(), L"multiple");
        } else if (GuiDialogHelpers::isName(argIn[k], L"InitialValue")) {
            initial = argIn[k + 1].asDoubleVector();
        }
    }
    if (items.empty()) {
        Error(L"Nelson:listdlg:listStringExpected", L"'%s' expected.", L"ListString");
    }

    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"list";
        request.title = title;
        request.message = prompt;
        request.items = items;
        request.multiSelect = multiple;
        for (double value : initial) {
            request.initialSelection.push_back(static_cast<int>(value));
        }
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            ArrayOfVector retval;
            if (!response.ok) {
                retval << ArrayOf::empty(0, 0);
                retval << ArrayOf::logicalScalar(false);
                return retval;
            }
            std::vector<double> selected;
            for (int idx : response.selectedIndices) {
                selected.push_back((double)idx);
            }
            retval << ArrayOf::doubleRowVector(selected);
            retval << ArrayOf::logicalScalar(true);
            return retval;
        }
    }
    Error(L"Nelson:listdlg:noDisplay",
        L"listdlg requires a graphical desktop, which is not available.");
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
