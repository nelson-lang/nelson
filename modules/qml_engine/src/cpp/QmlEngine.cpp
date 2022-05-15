//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QFile>
#include <QtCore/QPointer>
#include <QtCore/QScopedPointer>
#include <QtCore/QThread>
#include <QtQml/QQmlComponent>
#include <QtQml/QQmlContext>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlProperty>
#include <QtQuick/QQuickItem>
#include <QtQuick/QQuickView>
#include "QmlEngine.hpp"
#include "Error.hpp"
#include "MainGuiObject.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "characters_encoding.hpp"
#include "nelsonObject.h"
#include "QObjectHandleObjectAllocator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QmlEngine* QmlEngine::m_pInstance = nullptr;
QQmlEngine* qmlengine = nullptr;
//=============================================================================
void
terminateQmlEngine()
{
    if (qmlengine != nullptr) {
        QmlEngine::getInstance()->destroy();
    }
}
//=============================================================================
QmlEngine*
QmlEngine::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new QmlEngine();
    }
    return m_pInstance;
}
//=============================================================================
void
QmlEngine::destroy()
{
    if (qmlengine) {
        QThread* qthread = qmlengine->thread();
        qthread->quit();
        qthread->wait();
        qmlengine->deleteLater();
        qmlengine = nullptr;
    }
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
QObjectHandleObject*
QmlEngine::setData(const std::wstring& data)
{
    QPointer<QQmlComponent> component = new QQmlComponent(qmlengine);
    if (component != nullptr) {
        // clear cache to reload
        qmlengine->clearComponentCache();
        QString qdata = wstringToQString(data).toUtf8();
        component->setData(qdata.toUtf8(), QUrl::fromLocalFile(wstringToQString(L"")));
        QObject* topLevel = component->create();
        if (topLevel == nullptr) {
            if (component->isError()) {
                component->deleteLater();
                Error(QStringTowstring(component->errorString()));
            }
        } else {
            std::string classname = std::string(topLevel->metaObject()->className());
            if (topLevel->isWindowType() || (classname == "QQuickAbstractMessageDialog")) {
                QQuickWindow* QMainWindowParent = (QQuickWindow*)GetMainGuiObject();
                topLevel->setParent(QMainWindowParent);
            }
        }
        return QObjectHandleObjectAllocator(topLevel);
    }
    return nullptr;
}
//=============================================================================
QObjectHandleObject*
QmlEngine::loadQmlFile(const std::wstring& filename)
{
    QPointer<QQmlComponent> component = new QQmlComponent(qmlengine);
    if (component != nullptr) {
        // clear cache to reload
        qmlengine->clearComponentCache();
        component->loadUrl(QUrl::fromLocalFile(wstringToQString(filename)));
        QObject* topLevel = component->create();
        if (!topLevel && component->isError()) {
            component->deleteLater();
            Error(QStringTowstring(component->errorString()));
        }
        if (topLevel != nullptr) {
            std::string classname = std::string(topLevel->metaObject()->className());
            if (topLevel->isWindowType() || (classname == "QQuickAbstractMessageDialog")) {
                QQuickWindow* QMainWindowParent = (QQuickWindow*)GetMainGuiObject();
                topLevel->setParent(QMainWindowParent);
            }
            return QObjectHandleObjectAllocator(topLevel);
        }
    }
    return nullptr;
}
//=============================================================================
QObjectHandleObject*
QmlEngine::createQQuickView(const std::wstring& filename)
{
    QObject* topLevel = nullptr;
    QFile qf(wstringToQString(filename));

    if (qf.exists()) {
        QUrl qUrlLocal = QUrl::fromLocalFile(wstringToQString(filename));
        qmlengine->clearComponentCache();
        QQuickWindow* QMainWindowParent = (QQuickWindow*)GetMainGuiObject();
        QPointer<QQuickView> view = new QQuickView(qmlengine, nullptr);
        try {
            view->setSource(qUrlLocal);
        } catch (std::runtime_error&) {
        }
        topLevel = view->rootObject();
        if (topLevel == nullptr) {
            view->deleteLater();
            Error(_W("Cannot create QQuickView."));
        }
        topLevel->setParent(view); //-V595
        view->show();
        if (topLevel != nullptr) {
            topLevel = topLevel->parent();
            topLevel->setParent(QMainWindowParent);
            return QObjectHandleObjectAllocator(topLevel);
        }
        Error(_W("Cannot set parent."));

    } else {
        Error(_W("File does not exist:") + L"\n" + filename);
    }
    return QObjectHandleObjectAllocator(topLevel);
}
//=============================================================================
QmlEngine::QmlEngine()
{
    qmlengine = new QQmlEngine();
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    } else {
        nelsonObject* qobjnelson = new nelsonObject();
        QQmlContext* ctxt = qmlengine->rootContext();
        ctxt->setContextProperty("nelson", qobjnelson);
        QQmlEngine::setObjectOwnership(qobjnelson, QQmlEngine::CppOwnership);
    }
}
//=============================================================================
void
QmlEngine::clearComponentCache()
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    } else {
        qmlengine->clearComponentCache();
    }
}
//=============================================================================
wstringVector
QmlEngine::importPathList()
{
    wstringVector res;
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    } else {
        QStringList list = qmlengine->importPathList();
        for (int k = 0; k < list.size(); k++) {
            res.push_back(QStringTowstring(list[k]));
        }
    }
    return res;
}
//=============================================================================
wstringVector
QmlEngine::pluginPathList()
{
    wstringVector res;
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    if (qmlengine) {
        QStringList list = qmlengine->pluginPathList();
        for (int k = 0; k < list.size(); k++) {
            res.push_back(QStringTowstring(list[k]));
        }
    }
    return res;
}
//=============================================================================
void
QmlEngine::addImportPath(const std::wstring& path)
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    } else {
        qmlengine->addImportPath(wstringToQString(path));
    }
}
//=============================================================================
void
QmlEngine::addPluginPath(const std::wstring& path)
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    } else {
        qmlengine->addPluginPath(wstringToQString(path));
    }
}
//=============================================================================
std::wstring
QmlEngine::offlineStoragePath()
{
    std::wstring result;
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    } else {
        result = QStringTowstring(qmlengine->offlineStoragePath());
    }
    return result;
}
//=============================================================================
void
QmlEngine::setOfflineStoragePath(const std::wstring& dir)
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    } else {
        qmlengine->setOfflineStoragePath(wstringToQString(dir));
    }
}
//=============================================================================
void
QmlEngine::collectGarbage()
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    } else {
        qmlengine->collectGarbage();
    }
}
//=============================================================================
static void
errorMessage(QJSValue evaluationResult)
{
    if (evaluationResult.isError()) {
        std::wstring filename = QStringTowstring(evaluationResult.property("fileName").toString());
        std::wstring msg;
        if (filename.empty()) {
            msg = _W("Uncaught exception at line") + L" "
                + QStringTowstring(evaluationResult.property("lineNumber").toString()) + L"\n"
                + QStringTowstring(evaluationResult.toString());
        } else {
            msg = _W("Uncaught exception in") + L"\n" + filename + L"\n" + _W("at line") + L" "
                + QStringTowstring(evaluationResult.property("lineNumber").toString()) + L"\n"
                + QStringTowstring(evaluationResult.toString());
        }
        Error(msg);
    }
}
//=============================================================================
static QVariant
QJsValueToQVariant(QJSValue value)
{
    if (value.isBool()) {
        return value.toBool();
    }
    if (value.isNumber()) {
        return value.toNumber();
    }
    if (value.isVariant()) {
        return value.toVariant();
    }
    return value.toString();
}
//=============================================================================
ArrayOf
QmlEngine::evaluateString(const std::wstring& program, bool& withOuput)
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    QJSValue evaluationResult = qmlengine->evaluate(wstringToQString(program));
    errorMessage(evaluationResult);
    withOuput = !evaluationResult.isUndefined();
    if (withOuput) {
        return QVariantToArrayOf(QJsValueToQVariant(evaluationResult));
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
QmlEngine::evaluateFile(const std::wstring& filename, bool& withOuput)
{
    QFile qf(wstringToQString(filename));
    if (!qf.exists()) {
        Error(_W("File does not exist:") + L"\n" + filename);
    }
    qf.open(QFile::ReadOnly);
    QString source = QString::fromUtf8(qf.readAll());
    qf.close();
    ArrayOf res;
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    QJSValue evaluationResult = qmlengine->evaluate(source, wstringToQString(filename));
    errorMessage(evaluationResult);
    withOuput = !evaluationResult.isUndefined();
    if (withOuput) {
        return QVariantToArrayOf(QJsValueToQVariant(evaluationResult));
    }
    return ArrayOf();
}
//=============================================================================
}
//=============================================================================
