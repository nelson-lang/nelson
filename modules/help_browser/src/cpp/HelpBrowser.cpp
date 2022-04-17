//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
#include <QtCore/QFileInfo>
#include <QtHelp/QHelpEngineCore>
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
#include "HelpCollection.hpp"
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
wstringVector
HelpBrowser::getAttributes()
{
    return HelpCollection::getInstance()->getRegisteredFiles();
}
//=============================================================================
static std::wstring
getAssistantFilename()
{
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
HelpBrowser::isAvailable()
{
    return IsFile(getAssistantFilename());
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
    std::wstring cachedCollectionFile
        = HelpCollection::getInstance()->getNelsonCachedCollectionFullFilename();
    if (!IsFile(cachedCollectionFile)) {
        msg = _W("help collection file not found.");
        return false;
    }
    HelpCollection::getInstance()->stripNonExistingHelpFiles();
    QStringList args;
    args << QLatin1String("-quiet");
    args << QLatin1String("-enableRemoteControl");
    args << QLatin1String("-collectionFile");
    args << wstringToQString(cachedCollectionFile);
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
}
//=============================================================================
HelpBrowser::HelpBrowser() { qprocess = new QProcess(); }
//=============================================================================
void
HelpBrowser::registerHelpFiles(const wstringVector& filenames)
{
    HelpCollection::getInstance()->registerHelpFiles(filenames);
}
//=============================================================================
void
HelpBrowser::unregisterHelpFiles(const wstringVector& filenames)
{
    HelpCollection::getInstance()->unregisterHelpFiles(filenames);
}
//=============================================================================
void
HelpBrowser::clearCache()
{
    closeBrowser();
    HelpCollection::getInstance()->clearCache();
}
//=============================================================================
}
//=============================================================================
