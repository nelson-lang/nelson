//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QAbstractButton>
#include <QtWidgets/QPushButton>
#include "QuestionBox.hpp"
#include "GuiDialogProvider.hpp"
#include "GuiDialogsHelpers.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#ifdef _MSC_VER
#include "ForceWindowsTitleBarToDark.hpp"
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
QuestionBox(const std::wstring& title, const std::wstring& question, const std::wstring& button1,
    const std::wstring& button2, const std::wstring& button3, const std::wstring& defaultButton,
    int numberOfButtons)
{
    // Non-Qt front end (nelson-webview): serve the question through the registered
    // provider instead of a native QMessageBox (which has no event loop there).
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"question";
        request.title = title;
        request.message = question;
        request.icon = L"question";
        request.buttons.push_back(button1);
        if (numberOfButtons > 1) {
            request.buttons.push_back(button2);
        }
        if (numberOfButtons > 2) {
            request.buttons.push_back(button3);
        }
        request.defaultButton = defaultButton;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            return ArrayOf::characterArray(response.ok ? response.button : std::wstring());
        }
    }
    QMessageBox* msgBox = nullptr;
    try {
        msgBox = new QMessageBox(GuiDialogHelpers::mainWindowParent());
#ifdef _MSC_VER
        forceWindowsTitleBarToDark(msgBox->winId());
#endif
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    QString nelsonPath(
        Nelson::wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()));
    QString fileNameIcon = nelsonPath + "/resources/fibonacci.ico";
    QIcon qIcon(fileNameIcon);
    msgBox->setWindowIcon(qIcon);
    msgBox->setIcon(QMessageBox::Question);

    QPushButton* qButton1 = msgBox->addButton(wstringToQString(button1), QMessageBox::YesRole);
    if (numberOfButtons == 1) {
        msgBox->setDefaultButton(qButton1);
    }
    QPushButton* qButton2 = nullptr;
    QPushButton* qButton3 = nullptr;

    if (numberOfButtons > 1) {
        qButton2 = msgBox->addButton(wstringToQString(button2), QMessageBox::NoRole);
    }
    if (numberOfButtons > 2) {
        qButton3 = msgBox->addButton(wstringToQString(button3), QMessageBox::NoRole);
    }

    // Closing the dialog (X or Escape) must return an empty answer. Qt needs an escape button
    // for the close event to be accepted, so a hidden, unnamed one is registered. Answers are
    // resolved by comparing clickedButton(), never by button insertion order.
    QPushButton* qButtonEscape = msgBox->addButton(QString(), QMessageBox::RejectRole);
    qButtonEscape->hide();
    msgBox->setEscapeButton(qButtonEscape);

    if (defaultButton == button1) {
        msgBox->setDefaultButton(qButton1);
    }
    if (defaultButton == button2) {
        msgBox->setDefaultButton(qButton2);
    }
    if (defaultButton == button3) {
        msgBox->setDefaultButton(qButton3);
    }
    msgBox->setWindowTitle(wstringToQString(title));
    msgBox->setText(wstringToQString(question));
    msgBox->setModal(true);
    msgBox->exec();
    // Identify the answer from the button which was actually clicked: closing
    // the dialog (X, Escape) selects the escape button and returns "".
    std::wstring answer;
    QAbstractButton* clicked = msgBox->clickedButton();
    if (clicked == qButton1) {
        answer = button1;
    } else if (clicked != nullptr && clicked == qButton2) {
        answer = button2;
    } else if (clicked != nullptr && clicked == qButton3) {
        answer = button3;
    } else {
        answer = L"";
    }
    delete msgBox;
    return ArrayOf::characterArray(answer);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
