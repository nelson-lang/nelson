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
#include <QtCore/QString>
#include "HelpBrowser.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include "QStringConverter.hpp"
#include "HelpCollection.hpp"
#include "HelpViewerWindow.h"
#include "i18n.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static HelpViewerWindow* helpWindow = nullptr;
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
    closeBrowser();
    if (m_pInstance) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
void
HelpBrowser::showDocByModuleName(const std::wstring& name)
{
    std::wstring moduleIndex
        = L"qthelp://org.nelson.modules." + name + std::wstring(L".help/help/") + L"index.html";
    if (helpWindow) {
        helpWindow->setSource(moduleIndex);
    }
}
//=============================================================================
void
HelpBrowser::showDocByName(const std::wstring& name)
{
    wstringVector urls = HelpCollection::getInstance()->searchByName(name);
    if (urls.empty()) {
        helpWindow->search(name);
    } else {
        if (helpWindow) {
            helpWindow->setSource(urls[0]);
        }
    }
}
//=============================================================================
void
HelpBrowser::showDocByIdentifier(const std::wstring& identifier)
{
    wstringVector urls = HelpCollection::getInstance()->searchByIdentifier(identifier);
    if (helpWindow && !urls.empty()) {
        helpWindow->setSource(urls[0]);
    }
}
//=============================================================================
wstringVector
HelpBrowser::getAttributes()
{
    return HelpCollection::getInstance()->getRegisteredFiles();
}
//=============================================================================
bool
HelpBrowser::isVisible()
{
    if (helpWindow) {
        return helpWindow->isVisible();
    }
    return false;
}
//=============================================================================
void
HelpBrowser::show()
{
    if (helpWindow) {
        helpWindow->show();
        helpWindow->activateWindow();
        helpWindow->setWindowState(Qt::WindowActive);
    }
}
//=============================================================================
void
HelpBrowser::hide()
{
    if (helpWindow) {
        helpWindow->hide();
    }
}
//=============================================================================
bool
HelpBrowser::startBrowser(std::wstring& msg)
{
    if (helpWindow == nullptr) {
        std::wstring cachedCollectionFile
            = HelpCollection::getInstance()->getNelsonCachedCollectionFullFilename();
        if (!FileSystemWrapper::Path::is_regular_file(cachedCollectionFile)) {
            msg = _W("help collection file not found.");
            return false;
        }
        std::wstring mainUrl = L"qthelp://org.nelson.help/help/homepage.html";
        try {
            helpWindow = new HelpViewerWindow(cachedCollectionFile, mainUrl);
        } catch (std::bad_alloc& e) {
            helpWindow = nullptr;
            msg = utf8_to_wstring(e.what());
            return false;
        }
    }
#ifdef _MSC_VER
    Nelson::forceWindowsTitleBarToDark(helpWindow->winId());
#endif
    helpWindow->show();
    return true;
}
//=============================================================================
void
HelpBrowser::closeBrowser()
{
    if (helpWindow) {
        helpWindow->close();
        delete helpWindow;
        helpWindow = nullptr;
    }
}
//=============================================================================
HelpBrowser::HelpBrowser() = default;
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
void
HelpBrowser::setSource(const std::wstring& url)
{
    if (helpWindow) {
        helpWindow->setSource(url);
    }
}
//=============================================================================
}
//=============================================================================
