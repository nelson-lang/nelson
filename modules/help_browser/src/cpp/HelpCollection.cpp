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
#include <QtCore/QFileInfo>
#include <QtHelp/QHelpEngineCore>
#include <QtCore/QStandardPaths>
#include "QStringConverter.hpp"
#include "HelpCollection.hpp"
#include "GetNelsonPath.hpp"
#include "IsFile.hpp"
#include "RemoveDirectory.hpp"

//=============================================================================
namespace Nelson {
//=============================================================================
HelpCollection* HelpCollection::m_pInstance = nullptr;
static QHelpEngineCore* cachedCollection = nullptr;
//=============================================================================
HelpCollection*
HelpCollection::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new HelpCollection();
    }
    return m_pInstance;
}
//=============================================================================
void
HelpCollection::destroy()
{
    if (m_pInstance) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
    if (cachedCollection) {
        delete cachedCollection;
        cachedCollection = nullptr;
    }
}
//=============================================================================
HelpCollection::HelpCollection()
{
    std::wstring cachedCollectionFile = getNelsonCachedCollectionFullFilename();
    if (!IsFile(cachedCollectionFile)) {
        std::wstring collectionFile = getNelsonCollectionFullFilename();
        QHelpEngineCore* collection = new QHelpEngineCore(wstringToQString(collectionFile));
        if (collection) {
            collection->copyCollectionFile(wstringToQString(cachedCollectionFile));
            delete collection;
        }
    }
    cachedCollection = new QHelpEngineCore(wstringToQString(cachedCollectionFile));
    cachedCollection->setupData();
}
//=============================================================================
bool
HelpCollection::registerHelpFiles(const wstringVector& filenames)
{
    bool result = false;
    if (cachedCollection) {
        for (auto filename : filenames) {
            bool reg = cachedCollection->registerDocumentation(wstringToQString(filename));
            if (!reg) {
                result = false;
            }
        }
        if (!filenames.empty()) {
            result = true;
        }
    }
    return result;
}
//=============================================================================
bool
HelpCollection::unregisterHelpFiles(const wstringVector& filenames)
{
    if (cachedCollection) {
        for (auto filename : filenames) {
            const QString& namespaceName
                = QHelpEngineCore::namespaceName(wstringToQString(filename));
            bool unreg = cachedCollection->unregisterDocumentation(namespaceName);
            if (!unreg) {
                return false;
            }
        }
        if (!filenames.empty()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
wstringVector
HelpCollection::stripNonExistingHelpFiles()
{
    wstringVector strippedHelpFiles;
    if (cachedCollection) {
        const QStringList& namespaces = cachedCollection->registeredDocumentations();
        foreach (const QString& ns, namespaces) {
            QFileInfo fi(cachedCollection->documentationFileName(ns));
            if (!fi.exists() || !fi.isFile()) {
                cachedCollection->unregisterDocumentation(ns);
                strippedHelpFiles.push_back(QStringTowstring(fi.absoluteFilePath()));
            }
        }
    }
    return strippedHelpFiles;
}
//=============================================================================
wstringVector
HelpCollection::getRegisteredFiles()
{
    wstringVector filenames;
    if (cachedCollection) {
        const QStringList& namespaces = cachedCollection->registeredDocumentations();
        foreach (const QString& ns, namespaces) {
            QFileInfo fi(cachedCollection->documentationFileName(ns));
            if (fi.exists() || fi.isFile()) {
                filenames.push_back(QStringTowstring(fi.absoluteFilePath()));
            }
        }
    }
    return filenames;
}
//=============================================================================
bool
HelpCollection::clearCache()
{
    if (cachedCollection) {
        QStringList qStringListRegisteredDocumention = cachedCollection->registeredDocumentations();
        for (auto element : qStringListRegisteredDocumention) {
            QString filename = cachedCollection->documentationFileName(element);
            const QString& namespaceName = QHelpEngineCore::namespaceName(filename);
            cachedCollection->unregisterDocumentation(namespaceName);
        }
        std::wstring collectionPath = getNelsonCacheCollectionPath();
        delete cachedCollection;
        cachedCollection = nullptr;
        std::wstring msgError = L"";
        RemoveDirectory(collectionPath, true, msgError);

        std::wstring cachedCollectionFile = getNelsonCachedCollectionFullFilename();
        if (!IsFile(cachedCollectionFile)) {
            std::wstring collectionFile = getNelsonCollectionFullFilename();
            QHelpEngineCore* collection = new QHelpEngineCore(wstringToQString(collectionFile));
            if (collection) {
                collection->copyCollectionFile(wstringToQString(cachedCollectionFile));
                delete collection;
            }
        }
        cachedCollection = new QHelpEngineCore(wstringToQString(cachedCollectionFile));
        cachedCollection->setupData();
    }
    return false;
}
//=============================================================================
std::wstring
HelpCollection::getNelsonCachedCollectionFullFilename()
{
    std::wstring path = getNelsonCacheCollectionPath() + L"/" + getNelsonQhcFilename();
    return path;
}
//=============================================================================
std::wstring
HelpCollection::getNelsonCollectionFullFilename()
{
    std::wstring path
        = GetNelsonPath() + L"/modules/help_tools/resources/" + getNelsonQhcFilename();
    return path;
}
//=============================================================================
std::wstring
HelpCollection::getNelsonQhcFilename()
{
    return L"nelson_help_collection.qhc";
}
//=============================================================================
std::wstring
HelpCollection::getNelsonCacheCollectionPath()
{
#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
    QString cacheLocation = QStandardPaths::writableLocation(QStandardPaths::DataLocation);
#else
    QString cacheLocation = QStandardPaths::writableLocation(QStandardPaths::AppLocalDataLocation);
#endif
    return QStringTowstring(cacheLocation) + std::wstring(L"/help");
}
//=============================================================================
}
//=============================================================================
