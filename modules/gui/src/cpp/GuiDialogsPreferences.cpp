//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QAbstractButton>
#include <QtWidgets/QApplication>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QPushButton>
#include "GuiDialogs.hpp"
#include "GuiDialogProvider.hpp"
#include "GuiDialogsHelpers.hpp"
#include "QtPreferencesDialog.h"
#include "QStringConverter.hpp"
#include "i18n.hpp"
#ifdef _MSC_VER
#include "ForceWindowsTitleBarToDark.hpp"
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
UiPreferenceDialogResult
UiPreferenceDialog(const UiPreferenceDialogOptions& options)
{
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"preference";
        request.title = options.title;
        request.message = options.question;
        request.icon = L"question";
        request.buttons = options.buttonLabels;
        request.buttonValues = options.buttonValues;
        request.extraButtons = options.extraButtons;
        request.defaultButton = options.defaultButton;
        request.checkboxVisible = true;
        request.checkboxText = options.checkboxText;
        request.checkboxState = options.checkboxState;
        request.helpString = options.helpString;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            UiPreferenceDialogResult result;
            result.dialogOpened = true;
            result.checkboxChecked = response.checkboxChecked;
            result.helpRequested = response.helpRequested;
            result.buttonClicked = response.ok && !response.helpRequested;
            result.value = response.button;
            return result;
        }
    }

    QMessageBox msgBox(GuiDialogHelpers::mainWindowParent());
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(msgBox.winId());
#endif
    msgBox.setWindowTitle(wstringToQString(options.title));
    msgBox.setText(wstringToQString(options.question));
    msgBox.setIcon(QMessageBox::Question);
    msgBox.setModal(true);

    QCheckBox checkbox(wstringToQString(options.checkboxText));
    checkbox.setChecked(options.checkboxState);
    msgBox.setCheckBox(&checkbox);

    std::vector<QPushButton*> preferenceButtons;
    for (size_t k = 0; k < options.buttonLabels.size(); ++k) {
        QPushButton* button
            = msgBox.addButton(wstringToQString(options.buttonLabels[k]), QMessageBox::AcceptRole);
        preferenceButtons.push_back(button);
        if (options.defaultButton == options.buttonLabels[k]) {
            msgBox.setDefaultButton(button);
        }
    }

    std::vector<QPushButton*> extraButtons;
    for (const std::wstring& label : options.extraButtons) {
        QPushButton* button = msgBox.addButton(wstringToQString(label), QMessageBox::RejectRole);
        extraButtons.push_back(button);
        if (options.defaultButton == label) {
            msgBox.setDefaultButton(button);
        }
    }

    QPushButton* helpButton = nullptr;
    if (!options.helpString.empty()) {
        helpButton = msgBox.addButton(wstringToQString(options.helpString), QMessageBox::HelpRole);
    }

    QPushButton* escapeButton = msgBox.addButton(QString(), QMessageBox::RejectRole);
    escapeButton->hide();
    msgBox.setEscapeButton(escapeButton);
    msgBox.exec();

    UiPreferenceDialogResult result;
    result.dialogOpened = true;
    result.checkboxChecked = checkbox.isChecked();

    QAbstractButton* clicked = msgBox.clickedButton();
    if (helpButton != nullptr && clicked == helpButton) {
        result.helpRequested = true;
        return result;
    }
    for (size_t k = 0; k < preferenceButtons.size(); ++k) {
        if (clicked == preferenceButtons[k]) {
            result.buttonClicked = true;
            result.value = options.buttonValues[k];
            return result;
        }
    }
    for (size_t k = 0; k < extraButtons.size(); ++k) {
        if (clicked == extraButtons[k]) {
            result.buttonClicked = true;
            result.value = options.extraButtons[k];
            return result;
        }
    }
    return result;
}
//=============================================================================
void
ShowPreferencesDialog(const std::wstring& page)
{
    if (QApplication::instance() == nullptr) {
        return;
    }
    QtPreferencesDialog dialog(nullptr, GuiDialogHelpers::mainWindowParent());
    dialog.selectPage(wstringToQString(page));
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(dialog.winId());
#endif
    dialog.exec();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
