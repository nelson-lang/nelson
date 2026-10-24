//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GuiDialogs.hpp"
#include "GuiDialogsHelpers.hpp"
#include "ErrorID.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
openResolvedFile(Evaluator* eval, const std::wstring& filename);
//=============================================================================
static bool
isTextArgument(const ArrayOf& value);
//=============================================================================
static bool
isTruthyScalar(const ArrayOf& value);
//=============================================================================
void
UiOpen(Evaluator* eval, const ArrayOfVector& argIn)
{
    // uiopen(path[, direct]) opens the file without a dialog when it exists or 'direct' is
    // truthy; otherwise the file picker is shown.
    const bool haveFilename = !argIn.empty() && isTextArgument(argIn[0]);
    const bool direct = argIn.size() > 1 && isTruthyScalar(argIn[1]);
    if (haveFilename) {
        const std::wstring filename = argIn[0].asWideString();
        if (direct || FileSystemWrapper::Path(filename).is_regular_file()) {
            openResolvedFile(eval, filename);
            return;
        }
    }
    ArrayOfVector pickerArgs;
    if (haveFilename) {
        pickerArgs << argIn[0];
    }
    ArrayOfVector fileResult = UiGetFile(pickerArgs, false);
    if (fileResult.empty() || fileResult[0].isDoubleType()) {
        return;
    }
    const std::wstring filename = fileResult[1].asWideString() + fileResult[0].asWideString();
    openResolvedFile(eval, filename);
}
//=============================================================================
void
UiSave(Evaluator* eval, const ArrayOfVector& argIn)
{
    ArrayOfVector pickerArgs;
    pickerArgs << ArrayOf::characterArray(L"*.nh5");
    pickerArgs << ArrayOf::characterArray(_W("Save Workspace"));
    if (argIn.size() > 1) {
        pickerArgs << argIn[1];
    }
    ArrayOfVector fileResult = UiGetFile(pickerArgs, true);
    if (fileResult.empty() || fileResult[0].isDoubleType()) {
        return;
    }
    std::wstring filename = fileResult[1].asWideString() + fileResult[0].asWideString();
    ArrayOfVector saveArgs;
    saveArgs << ArrayOf::characterArray(filename);
    if (!argIn.empty()) {
        wstringVector names = GuiDialogHelpers::asTextList(argIn[0]);
        for (const std::wstring& name : names) {
            saveArgs << ArrayOf::characterArray(name);
        }
    }
    GuiDialogHelpers::findFunction("save")->evaluateFunction(eval, saveArgs, 0);
}
//=============================================================================
static bool
isTextArgument(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || (value.isStringArray() && value.isScalar());
}
//=============================================================================
static bool
isTruthyScalar(const ArrayOf& value)
{
    if (!value.isScalar() || !(value.isNumeric() || value.isLogical())) {
        return false;
    }
    return value.asDoubleScalar(true) != 0.0;
}
//=============================================================================
static void
openResolvedFile(Evaluator* eval, const std::wstring& filename)
{
    const std::wstring extension
        = StringHelpers::to_lower_copy(FileSystemWrapper::Path(filename).extension().wstring());
    // Block-diagram / model file formats are not supported: fail with a clear message
    // instead of passing the file to a loader that emits a cryptic error.
    if (extension == L".slx" || extension == L".mdl") {
        Error(L"Nelson:uiopen:unsupportedFileType", L"'%s' files are not supported.",
            extension.c_str());
    }
    if (extension == L".m") {
        ArrayOfVector editArgs;
        editArgs << ArrayOf::characterArray(filename);
        GuiDialogHelpers::findFunction("edit")->evaluateFunction(eval, editArgs, 0);
        return;
    }
    ArrayOfVector loadArgs;
    loadArgs << ArrayOf::characterArray(filename);
    GuiDialogHelpers::findFunction("load")->evaluateFunction(eval, loadArgs, 0);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
