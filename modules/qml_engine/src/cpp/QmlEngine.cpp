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
#include <QtQml/QQmlEngine>
#include <QtQml/QQmlComponent>
#include <QtQml/QQmlProperty>
#include "QmlEngine.hpp"
#include "QStringConverter.hpp"
#include "Exception.hpp"
#include "characters_encoding.hpp"
#include "nelsonObject.hpp"
#include "QVariantArrayOf.hpp"
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
	QmlHandleObject *QmlEngine::setData(std::wstring data)
	{
		QPointer<QQmlComponent> component = new QQmlComponent(qmlengine);
		if (component)
		{
			QString qdata = wstringToQString(data).toUtf8();
			component->setData(qdata.toUtf8(), QUrl::fromLocalFile(wstringToQString(L"")));
			QObject *topLevel = component->create();
			if (!topLevel && component->isError())
			{
				throw Exception(QStringTowstring(component->errorString()));
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
	QmlHandleObject *QmlEngine::loadQmlFile(std::wstring filename)
	{
		QPointer<QQmlComponent> component = new QQmlComponent(qmlengine);
		if (component)
		{
			component->loadUrl(QUrl::fromLocalFile(wstringToQString(filename)));
			QObject *topLevel = component->create();
			if (!topLevel && component->isError())
			{
				throw Exception(QStringTowstring(component->errorString()));
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
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		QJSValue nelsonObj = qmlengine->newQObject(new nelsonObject());
		qmlengine->globalObject().setProperty("nelson", nelsonObj);
	}
	//=============================================================================
	void QmlEngine::clearComponentCache()
	{
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		qmlengine->clearComponentCache();
	}
	//=============================================================================
	wstringVector QmlEngine::importPathList()
	{
		wstringVector res;
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		QStringList list = qmlengine->importPathList();
		for (int k = 0; k < list.size(); k++)
		{
			res.push_back(QStringTowstring(list[k]));
		}
		return res;
	}
	//=============================================================================
	wstringVector QmlEngine::pluginPathList()
	{
		wstringVector res;
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		QStringList list = qmlengine->pluginPathList();
		for (int k = 0; k < list.size(); k++)
		{
			res.push_back(QStringTowstring(list[k]));
		}
		return res;
	}
	//=============================================================================
	void QmlEngine::addImportPath(std::wstring path)
	{
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		qmlengine->addImportPath(wstringToQString(path));
	}
	//=============================================================================
	void QmlEngine::addPluginPath(std::wstring path)
	{
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		qmlengine->addPluginPath(wstringToQString(path));
	}
	//=============================================================================
	std::wstring QmlEngine::offlineStoragePath()
	{
		std::wstring res;
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		return QStringTowstring(qmlengine->offlineStoragePath());
	}
	//=============================================================================
	void QmlEngine::setOfflineStoragePath(std::wstring dir)
	{
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		qmlengine->setOfflineStoragePath(wstringToQString(dir));
	}
	//=============================================================================
	void QmlEngine::collectGarbage()
	{
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		qmlengine->collectGarbage();
	}
	//=============================================================================
	static void errorMessage(QJSValue evaluationResult)
	{
		if (evaluationResult.isError())
		{
			std::wstring filename = QStringTowstring(evaluationResult.property("fileName").toString());
			std::wstring msg;
			if (filename == L"")
			{
				msg = _W("Uncaught exception at line") + L" " + QStringTowstring(evaluationResult.property("lineNumber").toString()) + L"\n" + QStringTowstring(evaluationResult.toString());
			}
			else
			{
				msg = _W("Uncaught exception in") + L"\n" + filename + L"\n" + _W("at line") + L" " + QStringTowstring(evaluationResult.property("lineNumber").toString()) + L"\n" + QStringTowstring(evaluationResult.toString());
			}
			throw Exception(msg);
		}
	}
	//=============================================================================
	void QmlEngine::evaluateString(std::wstring program)
	{
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		QJSValue evaluationResult = qmlengine->evaluate(wstringToQString(program));
		errorMessage(evaluationResult);
	}
	//=============================================================================
	void QmlEngine::evaluateFile(std::wstring filename)
	{
		QFile qf(wstringToQString(filename));
		if (!qf.exists())
		{
			throw Exception(_W("File does not exist:") + L"\n" + filename);
		}
		qf.open(QFile::ReadOnly);
		QString source = QString::fromUtf8(qf.readAll());
		qf.close();

		ArrayOf res;
		if (qmlengine == nullptr)
		{
			throw Exception(_W("QML engine not initialized."));
		}
		QJSValue evaluationResult = qmlengine->evaluate(source, wstringToQString(filename));
		errorMessage(evaluationResult);
	}
	//=============================================================================
}
//=============================================================================
