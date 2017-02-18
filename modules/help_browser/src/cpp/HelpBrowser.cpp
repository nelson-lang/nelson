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
#include <QtGui/QDesktopServices>
#include <QtCore/QString>
#include <QtCore/QProcess>
#include <QtCore/QByteArray>
#include <QtCore/QThread>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/chrono/chrono.hpp>
#include <boost/thread/thread.hpp>
#include <boost/filesystem.hpp>
#include "HelpBrowser.hpp"
#include "QStringConverter.hpp"
#include "GetQtPath.hpp"
#include "RemoveDirectory.hpp"
#include "GetNelsonPath.hpp"
#include "GetNelsonBinariesPath.hpp"
#include "IsFile.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static QProcess *qprocess = nullptr;
    //=============================================================================
    static bool bOpenHomepage = true;
    //=============================================================================
    HelpBrowser *HelpBrowser::m_pInstance = nullptr;
    //=============================================================================
    HelpBrowser *HelpBrowser::getInstance()
    {
        if (m_pInstance == nullptr)
        {
            m_pInstance = new HelpBrowser();
        }
        return m_pInstance;
    }
    //=============================================================================
    void HelpBrowser::destroy()
    {
        packages.clear();
        if (m_pInstance)
        {
            delete m_pInstance;
            m_pInstance = nullptr;
        }
        if (qprocess)
        {
            delete qprocess;
            qprocess = nullptr;
        }
    }
    //=============================================================================
    void HelpBrowser::showDocByName(std::wstring name)
    {
        std::wstring command = std::wstring(L"activateKeyword") + std::wstring(L" ") + name;
        sendCommand(command);
    }
    //=============================================================================
    void HelpBrowser::showDocByIdentifier(std::wstring identifier)
    {
        std::wstring command = std::wstring(L"activateIdentifier") + std::wstring(L" ") + identifier;
        sendCommand(command);
    }
    //=============================================================================
    void HelpBrowser::sendCommand(const std::wstring &cmd)
    {
        QString CommandToSend = Nelson::wstringToQString(cmd);
        if (qprocess->state() != QProcess::Running)
        {
            return;
        }
        boost::this_thread::sleep_for(boost::chrono::milliseconds(uint64(200)));
        qprocess->write(CommandToSend.toLocal8Bit() + '\n');
        boost::this_thread::sleep_for(boost::chrono::milliseconds(uint64(1000)));
    }
    //=============================================================================
    bool HelpBrowser::startBrowser(std::wstring &msg)
    {
        if (qprocess->state() == QProcess::Running)
        {
            QProcess::ProcessError err = qprocess->error();
            msg = L"";
            return true;
        }
        std::wstring wapp;
#ifdef _MSC_VER
        wapp = GetNelsonBinariesPath() + L"/assistant.exe";
#else
#if !defined(Q_OS_MAC)
        wapp = GetNelsonBinariesPath() + std::wstring(L"/assistant");
#else
        wapp = GetNelsonBinariesPath() + std::wstring(L"/Assistant");
#endif
#endif
        if (!IsFile(wapp))
        {
#ifdef _MSC_VER
            wapp = GetQtPath(L"BinariesPath");
            wapp = wapp + std::wstring(L"/assistant.exe");
#else
            wapp = GetQtPath(L"BinariesPath");
#if !defined(Q_OS_MAC)
            wapp = wapp + std::wstring(L"/assistant");
#else
            wapp = wapp + std::wstring(L"/Assistant.app/Contents/MacOS/Assistant");
#endif
#endif
        }
        if (!IsFile(wapp))
        {
            msg = _W("Qt Assistant not found.");
            return false;
        }
        if (!IsFile(getQhcPath()))
        {
            msg = _W("help collection file not found.");
            return false;
        }
        QStringList args;
        args << QLatin1String("-quiet");
        args << QLatin1String("-enableRemoteControl");
        args << QLatin1String("-collectionFile");
        args << wstringToQString(getQhcPath());
        qprocess->start(wstringToQString(wapp), args);
        msg = L"";
        if (!qprocess->waitForStarted())
        {
            return true;
        }
        return true;
    }
    //=============================================================================
    void HelpBrowser::syncBrowser()
    {
        sendCommand(L"SyncContents");
    }
    //=============================================================================
    void HelpBrowser::closeBrowser()
    {
        if (qprocess->state() == QProcess::Running)
        {
            packages.clear();
            qprocess->terminate();
            qprocess->waitForFinished(3000);
        }
        bOpenHomepage = true;
    }
    //=============================================================================
    HelpBrowser::HelpBrowser()
    {
        qprocess = new QProcess();
    }
    //=============================================================================
    void HelpBrowser::registerHelpFile(std::wstring filename)
    {
        if (std::find(packages.begin(), packages.end(), filename) == packages.end())
        {
            packages.push_back(filename);
            std::wstring command = std::wstring(L"register ") + filename;
            if (boost::algorithm::ends_with(filename, L"org.nelson.help.qch"))
            {
                if (bOpenHomepage)
                {
                    command = command + L";" + L"setSource qthelp://org.nelson.help/help/homepage.html";
                    bOpenHomepage = false;
                }
            }
            sendCommand(command);
        }
    }
    //=============================================================================
    void HelpBrowser::unregisterHelpFile(std::wstring filename)
    {
        wstringVector::iterator result = find(packages.begin(), packages.end(), filename);
        if (result != packages.end())
        {
            packages.erase(result);
        }
        std::wstring command = std::wstring(L"unregister ") + filename;
        sendCommand(command);
    }
    //=============================================================================
    void HelpBrowser::clearCache()
    {
		closeBrowser();
        QString cacheLocation = QStandardPaths::writableLocation(QStandardPaths::DataLocation);
        std::wstring cachePath = QStringTowstring(cacheLocation) + std::wstring(L"/help");
        std::wstring msgError = L"";
        Nelson::RemoveDirectory(cachePath, true, msgError);
    }
    //=============================================================================
    std::wstring HelpBrowser::getQhcFilename()
    {
        return L"nelson_help_collection.qhc";
    }
    //=============================================================================
    std::wstring HelpBrowser::getQhcPath()
    {
        std::wstring path = GetNelsonPath() + L"/modules/help_tools/resources/" + getQhcFilename();
        return path;
    }
    //=============================================================================
}
//=============================================================================
