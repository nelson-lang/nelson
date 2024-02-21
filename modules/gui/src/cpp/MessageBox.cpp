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
#include <QtCore/QObject>
#include <QtWidgets/QAbstractButton>
#include <QtCore/QVariant>
#include "MessageBox.hpp"
#include "QObjectHandleObjectAllocator.hpp"
#include "QStringConverter.hpp"
#include "HandleManager.hpp"
#include "NelsonConfiguration.hpp"
#include "PredefinedErrorMessages.hpp"
#ifdef _MSC_VER
#include "ForceWindowsTitleBarToDark.hpp"
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
getSupportedMessageBoxIcons()
{
    wstringVector supportedIconNames = { L"none", L"help", L"warn", L"error" };
    return supportedIconNames;
}
//=============================================================================
wstringVector
getSupportedMessageBoxModes()
{
    wstringVector modes = { L"modal", L"non-modal", L"replace" };
    return modes;
}
//=============================================================================
static QMessageBox::Icon
getIconCode(const std::wstring& iconName)
{
    if (iconName == L"none" || iconName.empty()) {
        return QMessageBox::NoIcon;
    }
    if (iconName == L"help") {
        return QMessageBox::Information;
    } else if (iconName == L"warn") {
        return QMessageBox::Warning;
    } else if (iconName == L"error") {
        return QMessageBox::Critical;
    }
    return QMessageBox::NoIcon;
}
//=============================================================================
ArrayOf
MessageBox(const std::wstring& message, const std::wstring& title, const std::wstring& icon,
    const std::wstring& mode)
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
    bool modal = false;
    msgBox->setWindowIcon(qIcon);
    if (mode == L"modal") {
        modal = true;
    } else if (mode == L"non-modal") {
        modal = false;
    } else if (mode == L"replace") {
        std::vector<nelson_handle> qObjectNelsonHandles
            = HandleManager::getInstance()->getAllHandlesOfCategory(
                NLS_HANDLE_QOBJECT_CATEGORY_STR);
        for (auto qObjectNelsonHandle : qObjectNelsonHandles) {
            HandleGenericObject* hlObj
                = HandleManager::getInstance()->getPointer(qObjectNelsonHandle);
            QObject* qObj = (QObject*)hlObj->getPointer();
            if (qObj) {
                QVariant qVariantClassName = qObj->property("Classname");
                if (qVariantClassName.isValid()) {
                    bool isMessageBox = (qVariantClassName.toString() == QString("QMessageBox"));
                    if (isMessageBox) {
                        QMessageBox* msgBox = (QMessageBox*)qObj;
                        if (msgBox->windowTitle() == wstringToQString(title)) {
                            msgBox->setText(wstringToQString(message));
                            msgBox->setIcon(getIconCode(icon));
                            return ArrayOf::handleConstructor(QObjectHandleObjectAllocator(msgBox));
                        }
                    }
                }
            }
        }
    }
    msgBox->setIcon(getIconCode(icon));
    msgBox->setText(wstringToQString(message));
    msgBox->setWindowTitle(wstringToQString(title));
    msgBox->setModal(modal);
    msgBox->show();

    QObject::connect(msgBox->button(QMessageBox::Ok), &QAbstractButton::pressed, [msgBox]() {
        msgBox->close();
        msgBox->deleteLater();

        QObject* qObj = (QObject*)msgBox;
        std::vector<nelson_handle> qObjectNelsonHandles
            = HandleManager::getInstance()->getAllHandlesOfCategory(
                NLS_HANDLE_QOBJECT_CATEGORY_STR);
        for (auto qObjectNelsonHandle : qObjectNelsonHandles) {
            HandleGenericObject* hlObj
                = HandleManager::getInstance()->getPointer(qObjectNelsonHandle);
            if (qObj == hlObj->getPointer()) {
                hlObj->setPointer(nullptr);
            }
        }
    });
    return ArrayOf::handleConstructor(QObjectHandleObjectAllocator(msgBox));
}
//=============================================================================
}
//=============================================================================
