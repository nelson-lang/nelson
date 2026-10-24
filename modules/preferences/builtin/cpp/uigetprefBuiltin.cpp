//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uigetprefBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PreferencesBuiltinHelpers.hpp"
#include "PreferencesStore.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
#ifdef WITH_NELSON_GUI_PREFS
#include "GuiDialogs.hpp"
#endif
//=============================================================================
namespace Nelson::PreferencesGateway {
//=============================================================================
namespace {
    //=============================================================================
    wstringVector
    asTextList(const ArrayOf& value)
    {
        if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
            return { value.asWideString() };
        }
        if (value.isCellArrayOfCharacterVectors() || value.isStringArray()) {
            return value.asWideStringVector(false);
        }
        Error(L"Nelson:uigetpref:textExpected", L"Text expected.");
        return {};
    }
    //=============================================================================
    std::wstring
    asDialogText(const ArrayOf& value)
    {
        wstringVector lines = asTextList(value);
        std::wstring text;
        for (size_t k = 0; k < lines.size(); ++k) {
            if (k != 0) {
                text += L"\n";
            }
            text += lines[k];
        }
        return text;
    }
    //=============================================================================
    struct PreferenceButtons
    {
        wstringVector labels;
        wstringVector values;
    };
    //=============================================================================
    PreferenceButtons
    parseButtons(const ArrayOf& value)
    {
        PreferenceButtons result;
        wstringVector entries = asTextList(value);
        Dimensions dims = value.dimensions();
        if (dims.rows() == 2 && dims.columns() > 0 && entries.size() == dims.elementCount()) {
            for (indexType col = 0; col < dims.columns(); ++col) {
                result.labels.push_back(entries[col * 2]);
                result.values.push_back(entries[col * 2 + 1]);
            }
        } else {
            result.labels = entries;
            for (const std::wstring& label : entries) {
                result.values.push_back(lowerPreferenceText(label));
            }
        }
        if (result.labels.empty()) {
            Error(L"Nelson:uigetpref:buttonExpected", L"At least one preference button expected.");
        }
        return result;
    }
    //=============================================================================
    bool
    isTrueValue(const ArrayOf& value)
    {
        if (value.isLogical() || value.isNumeric()) {
            return value.asLogicalScalar(true) != 0;
        }
        return StringHelpers::iequals(value.asWideString(), L"true")
            || StringHelpers::iequals(value.asWideString(), L"on");
    }
    //=============================================================================
    struct UiGetPrefOptions
    {
        bool checkboxState = false;
        std::wstring checkboxString = _W("Do not show this dialog again.");
        wstringVector extraOptions;
        std::wstring defaultButton;
        std::wstring helpString;
        std::wstring helpFcn;
    };
    //=============================================================================
    UiGetPrefOptions
    parseOptions(const ArrayOfVector& argIn, size_t first)
    {
        UiGetPrefOptions options;
        for (size_t k = first; k + 1 < argIn.size(); k += 2) {
            std::wstring name = argIn[k].asWideString();
            const ArrayOf& value = argIn[k + 1];
            if (StringHelpers::iequals(name, L"CheckboxState")) {
                options.checkboxState = isTrueValue(value);
            } else if (StringHelpers::iequals(name, L"CheckboxString")) {
                options.checkboxString = value.asWideString();
            } else if (StringHelpers::iequals(name, L"ExtraOptions")) {
                options.extraOptions = asTextList(value);
            } else if (StringHelpers::iequals(name, L"DefaultButton")) {
                options.defaultButton = value.asWideString();
            } else if (StringHelpers::iequals(name, L"HelpString")) {
                options.helpString = value.asWideString();
            } else if (StringHelpers::iequals(name, L"HelpFcn")) {
                options.helpFcn = value.asWideString();
            } else {
                Error(L"Nelson:uigetpref:unknownOption", L"Unknown preference dialog option.");
            }
        }
        if (((argIn.size() - first) % 2) != 0) {
            Error(
                L"Nelson:preferences:nameValueNotPaired", L"Name-value arguments must be paired.");
        }
        return options;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
uigetprefBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 5);
    std::wstring group = Preferences::extractScalarName(argIn[0], L"group");
    std::wstring preference = Preferences::extractScalarName(argIn[1], L"preference");
    ArrayOf storedValue;
    if (Preferences::getPreferenceIfExists(group, preference, storedValue)
        && !isAskPreferenceValue(storedValue)) {
        ArrayOfVector retval;
        retval << storedValue;
        if (nLhs > 1) {
            retval << ArrayOf::logicalScalar(false);
        }
        return retval;
    }

    PreferenceButtons buttons = parseButtons(argIn[4]);
    UiGetPrefOptions options = parseOptions(argIn, 5);
    if (options.defaultButton.empty()) {
        options.defaultButton = buttons.values[0];
    }
    Preferences::registerUiPreference(group, preference);
    Preferences::setPreferences(group,
        Preferences::PreferenceNames { { preference }, Dimensions(1, 1), true },
        scalarArrayOfToArrayOfVector(ArrayOf::characterArray("ask")), false);

#ifdef WITH_NELSON_GUI_PREFS
    UiPreferenceDialogOptions dialogOptions;
    dialogOptions.title = argIn[2].asWideString();
    dialogOptions.question = asDialogText(argIn[3]);
    dialogOptions.buttonLabels = buttons.labels;
    dialogOptions.buttonValues = buttons.values;
    dialogOptions.extraButtons = options.extraOptions;
    dialogOptions.defaultButton = options.defaultButton;
    dialogOptions.checkboxText = options.checkboxString;
    dialogOptions.checkboxState = options.checkboxState;
    dialogOptions.helpString = options.helpString;
    dialogOptions.helpFcn = options.helpFcn;
    UiPreferenceDialogResult result = UiPreferenceDialog(dialogOptions);
    if (result.checkboxChecked && result.preferenceButton) {
        Preferences::setPreferences(group,
            Preferences::PreferenceNames { { preference }, Dimensions(1, 1), true },
            scalarArrayOfToArrayOfVector(ArrayOf::characterArray(result.value)), false);
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(result.value);
    if (nLhs > 1) {
        retval << ArrayOf::logicalScalar(result.dialogOpened);
    }
    return retval;
#else
    Error(L"Nelson:preferences:dialogNotAvailable",
        L"Preferences dialog is not available in this build.");
    return {};
#endif
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
