//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "uigetprefBuiltin.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
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
    bool
    isText(const ArrayOf& value)
    {
        return value.isRowVectorCharacterArray() || value.isScalarStringArray()
            || (value.isCharacterArray() && value.isEmpty());
    }
    //=============================================================================
    std::wstring
    textOf(const ArrayOf& value)
    {
        return value.isEmpty() ? std::wstring() : value.asWideString();
    }
    //=============================================================================
    std::wstring
    asScalarText(const ArrayOf& value, const std::wstring& name)
    {
        if (!isText(value)) {
            Error(L"Nelson:uigetpref:textExpected",
                L"%s must be a character vector or a string scalar.", name);
        }
        return textOf(value);
    }
    //=============================================================================
    wstringVector
    asTextList(const ArrayOf& value, const std::wstring& name)
    {
        if (isText(value)) {
            return { textOf(value) };
        }
        if (value.isCellArrayOfCharacterVectors() || value.isStringArray()) {
            return value.asWideStringVector(false);
        }
        Error(L"Nelson:uigetpref:textListExpected",
            L"%s must be text, a string array or a cell array of character vectors.", name);
        return { };
    }
    //=============================================================================
    wstringVector
    splitOnPipe(const std::wstring& text)
    {
        wstringVector parts;
        size_t start = 0;
        while (true) {
            size_t pos = text.find(L'|', start);
            if (pos == std::wstring::npos) {
                parts.push_back(text.substr(start));
                break;
            }
            parts.push_back(text.substr(start, pos - start));
            start = pos + 1;
        }
        return parts;
    }
    //=============================================================================
    std::wstring
    asDialogText(const ArrayOf& value)
    {
        // Each element and each '|' starts a new line.
        wstringVector lines = asTextList(value, L"question");
        std::wstring text;
        for (size_t k = 0; k < lines.size(); ++k) {
            if (k != 0) {
                text += L"\n";
            }
            text += lines[k];
        }
        std::replace(text.begin(), text.end(), L'|', L'\n');
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
        if (value.isCharacterArray()) {
            // 'Yes|No': one button per pipe-separated label.
            result.labels = splitOnPipe(asScalarText(value, L"pref_choices"));
            result.values = result.labels;
        } else {
            wstringVector entries = asTextList(value, L"pref_choices");
            Dimensions dims = value.dimensions();
            if (dims.is2D() && dims.rows() == 2 && dims.columns() > 0) {
                // Row 1 holds the stored values, row 2 the button labels.
                for (indexType col = 0; col < dims.columns(); ++col) {
                    result.values.push_back(entries[col * 2]);
                    result.labels.push_back(entries[col * 2 + 1]);
                }
            } else {
                result.labels = entries;
                result.values = entries;
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
        if (value.isLogical() && value.isScalar()) {
            return value.asLogicalScalar() != 0;
        }
        if (value.isNumeric() && value.isScalar() && !value.isComplex()) {
            return value.asDoubleScalar() != 0;
        }
        if (isText(value)) {
            std::wstring text = textOf(value);
            if (StringHelpers::iequals(text, L"true") || StringHelpers::iequals(text, L"on")) {
                return true;
            }
            if (StringHelpers::iequals(text, L"false") || StringHelpers::iequals(text, L"off")) {
                return false;
            }
        }
        Error(L"Nelson:uigetpref:invalidCheckboxState",
            L"CheckboxState must be a logical scalar, 'on' or 'off'.");
        return false;
    }
    //=============================================================================
    bool
    isHelpCallback(const ArrayOf& value)
    {
        if (isText(value) || value.isFunctionHandle()) {
            return true;
        }
        if (value.isCell() && !value.isEmpty()) {
            const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
            return elements[0].isFunctionHandle();
        }
        return false;
    }
    //=============================================================================
    struct UiGetPrefOptions
    {
        bool checkboxState = false;
        std::wstring checkboxString = _W("Do not show this dialog again.");
        wstringVector extraOptions;
        std::wstring defaultButton;
        bool hasDefaultButton = false;
        std::wstring helpString;
        std::wstring helpTopic = L"uigetpref";
        ArrayOf helpFcn;
    };
    //=============================================================================
    UiGetPrefOptions
    parseOptions(const ArrayOfVector& argIn, size_t first)
    {
        if (((argIn.size() - first) % 2) != 0) {
            Error(
                L"Nelson:preferences:nameValueNotPaired", L"Name-value arguments must be paired.");
        }
        UiGetPrefOptions options;
        for (size_t k = first; k + 1 < argIn.size(); k += 2) {
            std::wstring name = asScalarText(argIn[k], L"Option name");
            const ArrayOf& value = argIn[k + 1];
            if (StringHelpers::iequals(name, L"CheckboxState")) {
                options.checkboxState = isTrueValue(value);
            } else if (StringHelpers::iequals(name, L"CheckboxString")) {
                options.checkboxString = asScalarText(value, L"CheckboxString");
            } else if (StringHelpers::iequals(name, L"ExtraOptions")) {
                options.extraOptions = asTextList(value, L"ExtraOptions");
            } else if (StringHelpers::iequals(name, L"DefaultButton")) {
                options.defaultButton = asScalarText(value, L"DefaultButton");
                options.hasDefaultButton = true;
            } else if (StringHelpers::iequals(name, L"HelpString")) {
                options.helpString = asScalarText(value, L"HelpString");
            } else if (StringHelpers::iequals(name, L"HelpTopic")) {
                options.helpTopic = asScalarText(value, L"HelpTopic");
            } else if (StringHelpers::iequals(name, L"HelpFcn")) {
                if (!isHelpCallback(value)) {
                    Error(L"Nelson:uigetpref:invalidHelpFcn",
                        L"HelpFcn must be text, a function handle or a cell array whose first "
                        L"element is a function handle.");
                }
                options.helpFcn = value;
            } else if (StringHelpers::iequals(name, L"DialogProps")) {
                // Figure properties of a classic figure dialog: accepted but not used.
                if (!value.isStruct()) {
                    Error(L"Nelson:uigetpref:invalidDialogProps", L"DialogProps must be a struct.");
                }
            } else {
                Error(L"Nelson:uigetpref:unknownOption", L"Unknown preference dialog option.");
            }
        }
        return options;
    }
    //=============================================================================
#ifdef WITH_NELSON_GUI_PREFS
    void
    runHelpCallback(Evaluator* eval, const UiGetPrefOptions& options)
    {
        // Help callback errors are ignored so that the dialog stays usable.
        try {
            if (options.helpFcn.isEmpty()) {
                eval->evaluateString(L"doc('" + options.helpTopic + L"');\n", true);
            } else if (isText(options.helpFcn)) {
                eval->evaluateString(textOf(options.helpFcn) + L"\n", true);
            } else {
                ArrayOf fhArray = options.helpFcn;
                ArrayOfVector args;
                if (options.helpFcn.isCell()) {
                    const auto* elements
                        = static_cast<const ArrayOf*>(options.helpFcn.dataPointer());
                    fhArray = elements[0];
                    for (indexType k = 1; k < options.helpFcn.elementCount(); ++k) {
                        args.push_back(elements[k]);
                    }
                }
                function_handle fh = fhArray.asFunctionHandle();
                auto* anonymousFunction
                    = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
                if (anonymousFunction != nullptr) {
                    anonymousFunction->evaluateFunction(eval, args, 0);
                }
            }
        } catch (const NelsonException&) {
        }
    }
#endif
    //=============================================================================
    ArrayOf
    lowerAnswer(const ArrayOf& value)
    {
        if (isText(value)) {
            return ArrayOf::characterArray(lowerPreferenceText(textOf(value)));
        }
        return value;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
uigetprefBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 5);
    // All inputs are checked before any lookup or state change.
    std::wstring group = Preferences::extractScalarName(argIn[0], L"group");
    std::wstring preference = Preferences::extractScalarName(argIn[1], L"preference");
    std::wstring title = asScalarText(argIn[2], L"title");
    std::wstring question = asDialogText(argIn[3]);
    PreferenceButtons buttons = parseButtons(argIn[4]);
    UiGetPrefOptions options = parseOptions(argIn, 5);
    if (!options.hasDefaultButton) {
        options.defaultButton = buttons.labels[0];
    }

    ArrayOf storedValue;
    if (!Preferences::getPreferenceIfExists(group, preference, storedValue)) {
        // A missing preference is created with 'ask', like getpref with a default value.
        Preferences::setPreferences(group,
            Preferences::PreferenceNames { { preference }, Dimensions(1, 1), true },
            scalarArrayOfToArrayOfVector(ArrayOf::characterArray("ask")), false);
    } else if (!isAskPreferenceValue(storedValue)) {
        ArrayOfVector retval;
        retval << lowerAnswer(storedValue);
        if (nLhs > 1) {
            retval << ArrayOf::logicalScalar(false);
        }
        return retval;
    }

#ifdef WITH_NELSON_GUI_PREFS
    Preferences::registerUiPreference(group, preference);
    UiPreferenceDialogOptions dialogOptions;
    dialogOptions.title = title;
    dialogOptions.question = question;
    dialogOptions.buttonLabels = buttons.labels;
    dialogOptions.buttonValues = buttons.values;
    dialogOptions.extraButtons = options.extraOptions;
    dialogOptions.defaultButton = options.defaultButton;
    dialogOptions.checkboxText = options.checkboxString;
    dialogOptions.checkboxState = options.checkboxState;
    dialogOptions.helpString = options.helpString;
    UiPreferenceDialogResult result;
    while (true) {
        result = UiPreferenceDialog(dialogOptions);
        if (!result.helpRequested) {
            break;
        }
        // The help button closes the dialog: run the help, then show the dialog again.
        dialogOptions.checkboxState = result.checkboxChecked;
        runHelpCallback(eval, options);
    }
    if (!result.buttonClicked) {
        // Closed without a button: default button and initial check box state.
        result.value = options.defaultButton;
        result.checkboxChecked = options.checkboxState;
    }
    if (result.checkboxChecked
        && std::find(buttons.values.begin(), buttons.values.end(), result.value)
            != buttons.values.end()) {
        Preferences::setPreferences(group,
            Preferences::PreferenceNames { { preference }, Dimensions(1, 1), true },
            scalarArrayOfToArrayOfVector(ArrayOf::characterArray(result.value)), false);
    }
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(lowerPreferenceText(result.value));
    if (nLhs > 1) {
        retval << ArrayOf::logicalScalar(true);
    }
    return retval;
#else
    Error(L"Nelson:preferences:dialogNotAvailable",
        L"Preferences dialog is not available in this build.");
    return { };
#endif
}
//=============================================================================
} // namespace Nelson::PreferencesGateway
//=============================================================================
