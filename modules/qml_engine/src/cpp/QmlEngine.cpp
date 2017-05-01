//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <QtCore/qfile.h>
#include <QtCore/qpointer.h>
#include <QtCore/qscopedpointer.h>
#include <QtQuick/qquickitem.h>
#include <QtQml/QJSEngine>
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlComponent>
#include <QtQml/QQmlProperty>
#include <QtQuick/QQuickView>
#include "QmlEngine.hpp"
#include "QStringConverter.hpp"
#include "Exception.hpp"
#include "characters_encoding.hpp"
#include "nelsonObject.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    QmlEngine* QmlEngine::m_pInstance = nullptr;
    QQmlEngine *qmlengine = nullptr;
    //=============================================================================
    QmlEngine *QmlEngine::getInstance()
    {
        if (m_pInstance == nullptr)
        {
            m_pInstance = new QmlEngine();
        }
        return m_pInstance;
    }
    //=============================================================================
    QmlHandleObject *QmlEngine::loadQmlFile(std::wstring filename)
    {
        QPointer<QQmlComponent> component = new QQmlComponent(qmlengine, QUrl::fromLocalFile(wstringToQString(filename)));
        if (component)
        {
            QObject *topLevel = component->create();
            if (!topLevel && component->isError())
            {
                throw Exception(QStringTowstring(component->errorString()));
            }
            if (!topLevel->isWindowType())
            {
                QQuickView *view = new QQuickView();
                view->setSource(QUrl::fromLocalFile(wstringToQString(filename)));
                view->show();
                topLevel = view->rootObject();
                topLevel->setParent(view);
                if (!topLevel && component->isError())
                {
                    throw Exception(QStringTowstring(component->errorString()));
                }
            }
            QmlHandleObject * qmlHandle = nullptr;
            try
            {
                qmlHandle = new QmlHandleObject(topLevel);
            }
            catch (std::bad_alloc &e)
            {
                e.what();
                qmlHandle = nullptr;
                throw Exception(ERROR_MEMORY_ALLOCATION);
            }
            return qmlHandle;
        }
        return nullptr;
    }
    //=============================================================================
    QmlEngine::QmlEngine()
    {
        qmlengine = new QQmlEngine();
        if (qmlengine)
        {
            QJSValue nelsonObj = qmlengine->newQObject(new nelsonObject());
            qmlengine->globalObject().setProperty("nelson", nelsonObj);
        }
    }
    //=============================================================================
}
//=============================================================================
