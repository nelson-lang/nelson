//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <atomic>
#include <QtCore/QObject>
#include <QtGui/QIcon>
#include <QtWidgets/QAbstractButton>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QProgressDialog>
#include <QtWidgets/QPushButton>
#ifdef _MSC_VER
#include "ForceWindowsTitleBarToDark.hpp"
#endif
#include "GuiDialogs.hpp"
#include "GuiDialogProvider.hpp"
#include "GuiDialogsHelpers.hpp"
#include "QObjectHandleObject.hpp"
#include "QStringConverter.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    std::atomic<long long> progressDialogSequence { 0 };
    //=============================================================================
    bool
    isProgressProperty(const std::wstring& propertyName)
    {
        return propertyName == L"windowTitle" || propertyName == L"labelText"
            || propertyName == L"value" || propertyName == L"minimum" || propertyName == L"maximum"
            || propertyName == L"visible";
    }
    //=============================================================================
    class WebProgressDialogProxy : public QObject
    {
    public:
        WebProgressDialogProxy(
            const QString& title, const QString& message, int value, bool indeterminate)
            : m_id(++progressDialogSequence)
        {
            setProperty("windowTitle", title);
            setProperty("labelText", message);
            setProperty("value", value);
            setProperty("minimum", 0);
            setProperty("maximum", indeterminate ? 0 : 100);
            setProperty("visible", true);
        }

        bool
        publish()
        {
            GuiDialogRequest request;
            request.kind = L"progress";
            request.progressId = m_id;
            request.progressVisible = property("visible").toBool();
            request.title = QStringTowstring(property("windowTitle").toString());
            request.message = QStringTowstring(property("labelText").toString());
            request.progressValue = property("value").toInt();
            request.progressMinimum = property("minimum").toInt();
            request.progressMaximum = property("maximum").toInt();
            request.progressIndeterminate
                = request.progressMinimum == 0 && request.progressMaximum == 0;
            GuiDialogResponse response;
            return runGuiDialog(request, response);
        }

    private:
        long long m_id;
    };
    //=============================================================================
    class WebProgressDialogHandle : public QObjectHandleObject
    {
    public:
        explicit WebProgressDialogHandle(WebProgressDialogProxy* proxy) : QObjectHandleObject(proxy)
        {
        }

        void
        propertyChanged(const std::wstring& propertyName) override
        {
            if (!isProgressProperty(propertyName) || pointer() == nullptr) {
                return;
            }
            static_cast<WebProgressDialogProxy*>(pointer())->publish();
        }

        void
        onDelete(Evaluator*) override
        {
            if (pointer() == nullptr) {
                return;
            }
            auto* proxy = static_cast<WebProgressDialogProxy*>(pointer());
            proxy->setProperty("visible", false);
            proxy->publish();
            proxy->deleteLater();
            setPointer(nullptr);
        }
    };
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
UiAlert(const ArrayOfVector& argIn)
{
    std::wstring message = GuiDialogHelpers::asText(argIn[1]);
    std::wstring title = argIn[2].asWideString();
    std::wstring icon = L"info";
    for (size_t k = 3; k + 1 < argIn.size(); k += 2) {
        if (GuiDialogHelpers::isName(argIn[k], L"Icon")) {
            icon = argIn[k + 1].asWideString();
        }
    }
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"message";
        request.title = title;
        request.message = message;
        request.icon = icon;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            return ArrayOf::empty();
        }
    }
    QMessageBox box(GuiDialogHelpers::iconFromText(icon), wstringToQString(title),
        wstringToQString(message), QMessageBox::Ok, GuiDialogHelpers::mainWindowParent());
    box.setWindowIcon(QIcon(GuiDialogHelpers::nelsonIconPath()));
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(box.winId());
#endif
    box.exec();
    return ArrayOf::empty();
}
//=============================================================================
ArrayOf
UiConfirm(const ArrayOfVector& argIn)
{
    std::wstring message = GuiDialogHelpers::asText(argIn[1]);
    std::wstring title = argIn[2].asWideString();
    wstringVector options = { _W("OK"), _W("Cancel") };
    std::wstring defaultOption = options[0];
    for (size_t k = 3; k + 1 < argIn.size(); k += 2) {
        if (GuiDialogHelpers::isName(argIn[k], L"Options")) {
            options = GuiDialogHelpers::asTextList(argIn[k + 1]);
        } else if (GuiDialogHelpers::isName(argIn[k], L"DefaultOption")) {
            defaultOption = argIn[k + 1].asWideString();
        }
    }
    if (hasGuiDialogProvider()) {
        GuiDialogRequest request;
        request.kind = L"question";
        request.title = title;
        request.message = message;
        request.icon = L"question";
        request.buttons = options;
        request.defaultButton = defaultOption;
        GuiDialogResponse response;
        if (runGuiDialog(request, response)) {
            return ArrayOf::characterArray(response.ok ? response.button : std::wstring());
        }
    }
    QMessageBox box(QMessageBox::Question, wstringToQString(title), wstringToQString(message),
        QMessageBox::NoButton, GuiDialogHelpers::mainWindowParent());
    box.setWindowIcon(QIcon(GuiDialogHelpers::nelsonIconPath()));
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(box.winId());
#endif
    QPushButton* defaultButton = nullptr;
    for (const std::wstring& option : options) {
        QPushButton* button = box.addButton(wstringToQString(option), QMessageBox::ActionRole);
        if (option == defaultOption) {
            defaultButton = button;
        }
    }
    if (defaultButton != nullptr) {
        box.setDefaultButton(defaultButton);
    }
    box.exec();
    QAbstractButton* clicked = box.clickedButton();
    if (clicked == nullptr) {
        return ArrayOf::characterArray(L"");
    }
    return ArrayOf::characterArray(QStringTowstring(clicked->text()));
}
//=============================================================================
ArrayOf
UiProgressDialog(const ArrayOfVector& argIn)
{
    QString title("Progress");
    QString message;
    int value = 0;
    bool indeterminate = false;
    for (size_t k = 1; k + 1 < argIn.size(); k += 2) {
        if (GuiDialogHelpers::isName(argIn[k], L"Title")) {
            title = wstringToQString(argIn[k + 1].asWideString());
        } else if (GuiDialogHelpers::isName(argIn[k], L"Message")) {
            message = wstringToQString(GuiDialogHelpers::asText(argIn[k + 1]));
        } else if (GuiDialogHelpers::isName(argIn[k], L"Value")) {
            value = static_cast<int>(argIn[k + 1].asDoubleScalar() * 100.0);
        } else if (GuiDialogHelpers::isName(argIn[k], L"Indeterminate")
            && argIn[k + 1].asLogicalScalar()) {
            indeterminate = true;
        }
    }

    if (hasGuiDialogProvider()) {
        auto* proxy = new WebProgressDialogProxy(title, message, value, indeterminate);
        if (proxy->publish()) {
            proxy->setProperty("Classname", QString("QProgressDialog"));
            return ArrayOf::handle(new WebProgressDialogHandle(proxy));
        }
        delete proxy;
    }

    QProgressDialog* dialog
        = new QProgressDialog(QString(), QString(), 0, 100, GuiDialogHelpers::mainWindowParent());
    dialog->setWindowIcon(QIcon(GuiDialogHelpers::nelsonIconPath()));
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(dialog->winId());
#endif
    dialog->setWindowTitle(title);
    dialog->setLabelText(message);
    dialog->setValue(value);
    if (indeterminate) {
        dialog->setRange(0, 0);
    }
    dialog->show();
    return GuiDialogHelpers::qObjectHandle(dialog, QString("QProgressDialog"));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
