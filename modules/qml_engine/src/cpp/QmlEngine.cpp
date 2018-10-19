//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "QmlEngine.hpp"
#include "Error.hpp"
#include "MainGuiObject.hpp"
#include "QStringConverter.hpp"
#include "QVariantArrayOf.hpp"
#include "characters_encoding.hpp"
#include "nelsonObject.h"
#include <QtCore/QFile>
#include <QtCore/QPointer>
#include <QtCore/QScopedPointer>
#include <QtQml/QQmlComponent>
#include <QtQml/QQmlContext>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlProperty>
#include <QtQuick/QQuickItem>
#include <QtQuick/QQuickView>
//=============================================================================
namespace Nelson {
//=============================================================================
QmlEngine* QmlEngine::m_pInstance = nullptr;
QQmlEngine* qmlengine = nullptr;
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
static QmlHandleObject*
allocateQmlHandle(QObject* qobj)
{
    QmlHandleObject* qmlHandle = nullptr;
    try {
        qmlHandle = new QmlHandleObject(qobj);
    } catch (const std::bad_alloc& e) {
        e.what();
        qmlHandle = nullptr;
        Error(ERROR_MEMORY_ALLOCATION);
    }
    return qmlHandle;
}
//=============================================================================
QmlHandleObject*
QmlEngine::setData(std::wstring data)
{
    QPointer<QQmlComponent> component = new QQmlComponent(qmlengine);
    if (component) {
        // clear cache to reload
        qmlengine->clearComponentCache();
        QString qdata = wstringToQString(data).toUtf8();
        component->setData(qdata.toUtf8(), QUrl::fromLocalFile(wstringToQString(L"")));
        QObject* topLevel = component->create();
        if (!topLevel && component->isError()) {
            component->deleteLater();
            Error(QStringTowstring(component->errorString()));
        }
        std::string classname = std::string(topLevel->metaObject()->className());
        if (topLevel->isWindowType() || (classname == "QQuickAbstractMessageDialog")) {
            QQuickWindow* QMainWindowParent = (QQuickWindow*)GetMainGuiObject();
            topLevel->setParent(QMainWindowParent);
        }
        return allocateQmlHandle(topLevel);
    }
    return nullptr;
}
//=============================================================================
QmlHandleObject*
QmlEngine::loadQmlFile(std::wstring filename)
{
    QPointer<QQmlComponent> component = new QQmlComponent(qmlengine);
    if (component) {
        // clear cache to reload
        qmlengine->clearComponentCache();
        component->loadUrl(QUrl::fromLocalFile(wstringToQString(filename)));
        QObject* topLevel = component->create();
        if (!topLevel && component->isError()) {
            component->deleteLater();
            Error(QStringTowstring(component->errorString()));
        }
        std::string classname = std::string(topLevel->metaObject()->className());
        if (topLevel->isWindowType() || (classname == "QQuickAbstractMessageDialog")) {
            QQuickWindow* QMainWindowParent = (QQuickWindow*)GetMainGuiObject();
            topLevel->setParent(QMainWindowParent);
        }
        return allocateQmlHandle(topLevel);
    }
    return nullptr;
}
//=============================================================================
QmlHandleObject*
QmlEngine::createQQuickView(std::wstring filename)
{
    QObject* topLevel = nullptr;
    QFile qf(wstringToQString(filename));
    if (qf.exists()) {
        qmlengine->clearComponentCache();
        QQuickWindow* QMainWindowParent = (QQuickWindow*)GetMainGuiObject();
        QPointer<QQuickView> view = new QQuickView(qmlengine, nullptr);
        view->setSource(QUrl::fromLocalFile(wstringToQString(filename)));
        topLevel = view->rootObject();
        if (topLevel == nullptr) {
            view->deleteLater();
            Error(_W("Cannot create QQuickView."));
        }
        topLevel->setParent(view);
        view->show();
        topLevel = topLevel->parent();
        topLevel->setParent(QMainWindowParent);
        return allocateQmlHandle(topLevel);
    } else {
        Error(_W("File does not exist:") + L"\n" + filename);
    }
    return allocateQmlHandle(topLevel);
}
//=============================================================================
QmlEngine::QmlEngine()
{
    qmlengine = new QQmlEngine();
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    nelsonObject* qobjnelson = new nelsonObject();
    QQmlContext* ctxt = qmlengine->rootContext();
    ctxt->setContextProperty("nelson", qobjnelson);
    QQmlEngine::setObjectOwnership(qobjnelson, QQmlEngine::CppOwnership);
}
//=============================================================================
void
QmlEngine::clearComponentCache()
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    qmlengine->clearComponentCache();
}
//=============================================================================
wstringVector
QmlEngine::importPathList()
{
    wstringVector res;
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    QStringList list = qmlengine->importPathList();
    for (int k = 0; k < list.size(); k++) {
        res.push_back(QStringTowstring(list[k]));
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
    QStringList list = qmlengine->pluginPathList();
    for (int k = 0; k < list.size(); k++) {
        res.push_back(QStringTowstring(list[k]));
    }
    return res;
}
//=============================================================================
void
QmlEngine::addImportPath(std::wstring path)
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    qmlengine->addImportPath(wstringToQString(path));
}
//=============================================================================
void
QmlEngine::addPluginPath(std::wstring path)
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    qmlengine->addPluginPath(wstringToQString(path));
}
//=============================================================================
std::wstring
QmlEngine::offlineStoragePath()
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    return QStringTowstring(qmlengine->offlineStoragePath());
}
//=============================================================================
void
QmlEngine::setOfflineStoragePath(std::wstring dir)
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    qmlengine->setOfflineStoragePath(wstringToQString(dir));
}
//=============================================================================
void
QmlEngine::collectGarbage()
{
    if (qmlengine == nullptr) {
        Error(_W("QML engine not initialized."));
    }
    qmlengine->collectGarbage();
}
//=============================================================================
static void
errorMessage(QJSValue evaluationResult)
{
    if (evaluationResult.isError()) {
        std::wstring filename = QStringTowstring(evaluationResult.property("fileName").toString());
        std::wstring msg;
        if (filename == L"") {
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
QmlEngine::evaluateString(std::wstring program, bool& withOuput)
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
QmlEngine::evaluateFile(std::wstring filename, bool& withOuput)
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
