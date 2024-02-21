//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QAbstractButton>
#include <QtWidgets/QPushButton>
#include "QuestionBox.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
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
    QMessageBox* msgBox = nullptr;
    try {
        msgBox = new QMessageBox(nullptr);
#ifdef _MSC_VER
        forceWindowsTitleBarToDark(msgBox->winId());
#endif
    } catch (const std::bad_alloc&) {
        Error(ERROR_MEMORY_ALLOCATION);
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
    QPushButton* qButtonX = nullptr;

    if (numberOfButtons > 1) {
        qButton2 = msgBox->addButton(wstringToQString(button2), QMessageBox::NoRole);
    }
    if (numberOfButtons > 2) {
        qButton3 = msgBox->addButton(wstringToQString(button3), QMessageBox::NoRole);
    }

    qButtonX
        = msgBox->addButton(wstringToQString(L"__REJECTED_X_BUTTON_"), QMessageBox::RejectRole);
    qButtonX->hide();

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
    msgBox->show();
    int buttonPressed = msgBox->exec();
    std::wstring answer;
    if (buttonPressed >= numberOfButtons) {
        answer = L"";
    } else {
        switch (buttonPressed) {
        case 0: {
            answer = button1;
        } break;
        case 1: {
            answer = button2;
        } break;
        case 2: {
            answer = button3;
        } break;
        default: {
            answer = L"";
        } break;
        }
    }
    delete msgBox;
    return ArrayOf::characterArrayConstructor(answer);
}
//=============================================================================
}
//=============================================================================
