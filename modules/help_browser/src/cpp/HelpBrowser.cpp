//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include <QtCore/QStandardPaths>
#include <QtCore/QtGlobal>
#include <QtCore/QByteArray>
#include <QtCore/QProcess>
#include <QtCore/QString>
#include <QtCore/QThread>
#include <QtCore/QVariant>
#include <QtGui/QDesktopServices>
#include <QtSql/QSqlDatabase>
#include <QtSql/QSqlQuery>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/chrono/chrono.hpp>
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>
#include <boost/thread/thread.hpp>
#include "HelpBrowser.hpp"
#include "GetNelsonBinariesPath.hpp"
#include "GetNelsonPath.hpp"
#include "GetQtPath.hpp"
#include "IsFile.hpp"
#include "characters_encoding.hpp"
#include "QStringConverter.hpp"
#include "RemoveDirectory.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static QProcess* qprocess = nullptr;
//=============================================================================
static bool bOpenHomepage = true;
//=============================================================================
HelpBrowser* HelpBrowser::m_pInstance = nullptr;
//=============================================================================
HelpBrowser*
HelpBrowser::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new HelpBrowser();
    }
    return m_pInstance;
}
//=============================================================================
void
HelpBrowser::destroy()
{
    if (m_pInstance) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
    if (qprocess) {
        delete qprocess;
        qprocess = nullptr;
    }
}
//=============================================================================
void
HelpBrowser::showDocByName(const std::wstring& name)
{
    std::wstring command = std::wstring(L"activateKeyword") + std::wstring(L" ") + name;
    sendCommand(command);
}
//=============================================================================
void
HelpBrowser::showDocByIdentifier(const std::wstring& identifier)
{
    std::wstring command = std::wstring(L"activateIdentifier") + std::wstring(L" ") + identifier;
    sendCommand(command);
}
//=============================================================================
void
HelpBrowser::sendCommand(const std::wstring& cmd)
{
    QString CommandToSend = Nelson::wstringToQString(cmd);
    if (qprocess->state() != QProcess::Running) {
        return;
    }
    qprocess->write(CommandToSend.toLocal8Bit() + '\n');
    boost::this_thread::sleep_for(boost::chrono::milliseconds(uint64(1000)));
}
//=============================================================================
std::wstring
HelpBrowser::getCachePath()
{
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
    QString cacheLocation = QStandardPaths::writableLocation(QStandardPaths::DataLocation);
#else
    QString cacheLocation = QStandardPaths::writableLocation(QStandardPaths::AppLocalDataLocation);
#endif
    return QStringTowstring(cacheLocation) + std::wstring(L"/help");
}
//=============================================================================
wstringVector
HelpBrowser::getAttributes()
{
    wstringVector attributes;
#if QT_VERSION > QT_VERSION_CHECK(5, 5, 0)
    boost::this_thread::sleep_for(boost::chrono::milliseconds(uint64(500)));
    std::wstring database_path = getCacheFile();
    if (!database_path.empty()) {
        QSqlDatabase m_db;
        m_db = QSqlDatabase::addDatabase("QSQLITE");
        m_db.setDatabaseName(wstringToQString(database_path));
        if (m_db.open()) {
            QSqlQuery query("SELECT attributes FROM contents");
            while (query.next()) {
                QString qattribute = query.value(0).toString();
                std::wstring attribute = QStringTowstring(qattribute);
                if (std::find(attributes.begin(), attributes.end(), attribute)
                    == attributes.end()) {
                    attributes.push_back(attribute);
                }
            }
            m_db.close();
        }
    }
#endif
    return attributes;
}
//=============================================================================
static
std::wstring
getAssistantFilename() {
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
    if (!IsFile(wapp)) {
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
#if !defined(_MSC_VER) && !defined(Q_OS_MAC)
    if (!IsFile(wapp)) {
        const char* qtBinaries = getenv("QTDIR_BINARIES");
        if (qtBinaries) {
            wapp = utf8_to_wstring(qtBinaries) + std::wstring(L"/assistant");
        }
    }
    if (!IsFile(wapp)) {
        wapp = std::wstring(L"/usr/lib/x86_64-linux-gnu/qt5/bin/assistant");
    }
#endif
    return wapp;
}
//=============================================================================
bool
HelpBrowser::startBrowser(std::wstring& msg)
{
    if (qprocess->state() == QProcess::Running) {
        QProcess::ProcessError err = qprocess->error();
        msg.clear();
        return true;
    }
    std::wstring wapp = getAssistantFilename();
    if (!IsFile(wapp)) {
        msg = _W("Qt Assistant not found.");
        return false;
    }
    if (!IsFile(getQhcPath())) {
        msg = _W("help collection file not found.");
        return false;
    }
    QStringList args;
    args << QLatin1String("-quiet");
    args << QLatin1String("-enableRemoteControl");
    args << QLatin1String("-collectionFile");
    args << wstringToQString(getQhcPath());
    qprocess->start(wstringToQString(wapp), args);
    msg.clear();
    if (!qprocess->waitForStarted()) {
        return false;
    }
    return true;
}
//=============================================================================
void
HelpBrowser::syncBrowser()
{
    sendCommand(L"SyncContents");
}
//=============================================================================
void
HelpBrowser::closeBrowser()
{
    if (qprocess->state() == QProcess::Running) {
        qprocess->terminate();
        qprocess->waitForFinished(3000);
    }
    bOpenHomepage = true;
}
//=============================================================================
HelpBrowser::HelpBrowser() { qprocess = new QProcess(); }
//=============================================================================
void
HelpBrowser::registerHelpFile(const std::wstring& filename)
{
    std::wstring command = std::wstring(L"register ") + filename;
    if (boost::algorithm::ends_with(filename, L"org.nelson.help.qch")) {
        if (bOpenHomepage) {
            command = command + L";" + L"setSource qthelp://org.nelson.help/help/homepage.html";
            bOpenHomepage = false;
        }
    }
    sendCommand(command);
}
//=============================================================================
void
HelpBrowser::unregisterHelpFile(const std::wstring& filename)
{
    std::wstring command = std::wstring(L"unregister ") + filename;
    sendCommand(command);
}
//=============================================================================
void
HelpBrowser::clearCache()
{
    closeBrowser();
    std::wstring cachePath = getCachePath();
    std::wstring msgError = L"";
    RemoveDirectory(cachePath, true, msgError);
}
//=============================================================================
std::wstring
HelpBrowser::getQhcFilename()
{
    return L"nelson_help_collection.qhc";
}
//=============================================================================
std::wstring
HelpBrowser::getQhcPath()
{
    std::wstring path = GetNelsonPath() + L"/modules/help_tools/resources/" + getQhcFilename();
    return path;
}
//=============================================================================
std::wstring
HelpBrowser::getCacheFile()
{
    std::wstring database_path = getCachePath() + L"/.nelson_help_collection";
#ifdef _MSC_VER
#else
#endif
    boost::filesystem::path base_dir(database_path);
    bool isdir = false;
    try {
        isdir = boost::filesystem::exists(base_dir) && boost::filesystem::is_directory(base_dir);
    } catch (const boost::filesystem::filesystem_error&) {
        isdir = false;
    }
    if (isdir) {
        for (boost::filesystem::recursive_directory_iterator iter(base_dir), end; iter != end;
             ++iter) {
            std::wstring name = iter->path().filename().wstring();
            if (name == L"fts") {
                return database_path + L"/" + name;
            }
        }
    }
    return L"";
}
//=============================================================================
}
//=============================================================================
