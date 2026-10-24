//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <nlohmann/json.hpp>
#include <new>
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
#include "characters_encoding.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NelsonConfiguration* NelsonConfiguration::m_pInstance = nullptr;
//=============================================================================
NelsonConfiguration::NelsonConfiguration()
{
    InterruptPending.clear();
    mainInterruptPending.store(false, std::memory_order_relaxed);
    currentOverloadLevelCompatibility = NLS_OVERLOAD_ALL_TYPES;
    currentNumericFormatDisplay = NLS_NUMERIC_FORMAT_SHORT;
    currentLineSpacingDisplay = NLS_LINE_SPACING_LOOSE;
    currentTruncateMatricesDisplay = true;
    modulesProtected = false;
    nelsonRootPath = L"";
    ipcEnabled = false;
    fileWatcherEnabled = true;
    mainGuiObject = nullptr;
    mainEvaluator = nullptr;
    FileManager = nullptr;
    RandomEngine = nullptr;
    HistoryManager = nullptr;
    engineMode = 0;
    lastErrorException.clear();
    lastWarningException.clear();
    currentAxesOnClick = true;
    currentFigureOnClick = true;
    currentLocale = getDefaultLocale();
    useEmbeddedEditorFlag = true;
    editorCommandLine = L"";
    ompThreshold = OMP_RUNTIME_DEFAULT_THRESHOLD;
    ompEnable = true;
    m_isClosing = false;
}
//=============================================================================
NelsonConfiguration*
NelsonConfiguration::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new NelsonConfiguration();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
NelsonConfiguration::destroy()
{
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
bool
NelsonConfiguration::getInterruptPending(size_t evaluatorID)
{
    if (evaluatorID == 0) {
        return mainInterruptPending.load(std::memory_order_relaxed);
    }
    std::lock_guard<std::mutex> lock(InterruptPendingMutex);
    auto it = InterruptPending.find(evaluatorID);
    if (it == InterruptPending.end()) {
        return false;
    }
    return it->second;
}
//=============================================================================
bool
NelsonConfiguration::setInterruptPending(bool bInterruptPending, size_t evaluatorID)
{
    bool bPrevious = getInterruptPending(evaluatorID);
    if (evaluatorID == 0) {
        mainInterruptPending.store(bInterruptPending, std::memory_order_relaxed);
        return bPrevious;
    }
    std::lock_guard<std::mutex> lock(InterruptPendingMutex);
    InterruptPending[evaluatorID] = bInterruptPending;
    return bPrevious;
}
//=============================================================================
bool
NelsonConfiguration::isProfilerActive()
{
    return m_profilerActive.load(std::memory_order_relaxed);
}
//=============================================================================
void
NelsonConfiguration::setProfilerActive(bool active)
{
    m_profilerActive.store(active, std::memory_order_relaxed);
}
//=============================================================================
NumericFormatDisplay
NelsonConfiguration::setNumericFormatDisplay(NumericFormatDisplay desiredOutputFormatDisplay)
{
    NumericFormatDisplay previousOutputFormatDisplay = currentNumericFormatDisplay;
    currentNumericFormatDisplay = desiredOutputFormatDisplay;
    return previousOutputFormatDisplay;
}
//=============================================================================
NumericFormatDisplay
NelsonConfiguration::getNumericFormatDisplay()
{
    return currentNumericFormatDisplay;
}
//=============================================================================
LineSpacingDisplay
NelsonConfiguration::setLineSpacingDisplay(LineSpacingDisplay desiredLineSpacingDisplay)
{
    LineSpacingDisplay previousLineSpacingDisplay = currentLineSpacingDisplay;
    currentLineSpacingDisplay = desiredLineSpacingDisplay;
    return previousLineSpacingDisplay;
}
//=============================================================================
LineSpacingDisplay
NelsonConfiguration::getLineSpacingDisplay()
{
    return currentLineSpacingDisplay;
}
//=============================================================================
bool
NelsonConfiguration::setTruncateMatricesDisplay(bool truncateMatrices)
{
    bool previousTruncateMatricesDisplay = currentTruncateMatricesDisplay;
    currentTruncateMatricesDisplay = truncateMatrices;
    return previousTruncateMatricesDisplay;
}
//=============================================================================
bool
NelsonConfiguration::getTruncateMatricesDisplay()
{
    return currentTruncateMatricesDisplay;
}
//=============================================================================
void
NelsonConfiguration::enableModulesProtection()
{
    modulesProtected = true;
}
//=============================================================================
void
NelsonConfiguration::disableModulesProtection()
{
    modulesProtected = false;
}
//=============================================================================
bool
NelsonConfiguration::isModulesProtected()
{
    return modulesProtected;
}
//=============================================================================
void
NelsonConfiguration::enableFileWatcher()
{
    fileWatcherEnabled = true;
}
//=============================================================================
void
NelsonConfiguration::disableFileWatcher()
{
    fileWatcherEnabled = false;
}
//=============================================================================
bool
NelsonConfiguration::isFileWatcherEnabled()
{
    return fileWatcherEnabled;
}
//=============================================================================
void
NelsonConfiguration::setNelsonRootDirectory(const std::wstring& nelsonroot)
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    nelsonRootPath = nelsonroot;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonRootDirectory()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    return nelsonRootPath;
}
//=============================================================================
void
NelsonConfiguration::setNelsonBinaryDirectory(const std::wstring& directory)
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    nelsonBinaryPath = directory;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonBinaryDirectory()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    return nelsonBinaryPath;
}
//=============================================================================
void
NelsonConfiguration::setNelsonLibraryDirectory(const std::wstring& directory)
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    nelsonLibraryPath = directory;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonLibraryDirectory()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    return nelsonLibraryPath;
}
//=============================================================================
void
NelsonConfiguration::setNelsonModulesDirectory(const std::wstring& directory)
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    nelsonModulesPath = directory;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonModulesDirectory()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    return nelsonModulesPath;
}
//=============================================================================
void
NelsonConfiguration::setNelsonPreferencesDirectory(const std::wstring& directory)
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    nelsonPreferencesPath = directory;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonPreferencesDirectory()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    return nelsonPreferencesPath;
}
//=============================================================================
void
NelsonConfiguration::setMainIOInterface(void* IOInterface)
{
    this->mainInputOutputInterface = IOInterface;
}
//=============================================================================
void*
NelsonConfiguration::getMainIOInterface()
{
    return this->mainInputOutputInterface;
}
//=============================================================================
void
NelsonConfiguration::setMainGuiObject(void* mainGuiObject)
{
    this->mainGuiObject = mainGuiObject;
}
//=============================================================================
void*
NelsonConfiguration::getMainGuiObject()
{
    return this->mainGuiObject;
}
//=============================================================================
void
NelsonConfiguration::setMainEvaluator(void* mainEvaluator)
{
    this->mainEvaluator = mainEvaluator;
}
//=============================================================================
void*
NelsonConfiguration::getMainEvaluator()
{
    return this->mainEvaluator;
}
//=============================================================================
void
NelsonConfiguration::setFileManager(void* filemanager)
{
    this->FileManager = filemanager;
}
//=============================================================================
void*
NelsonConfiguration::getFileManager()
{
    return this->FileManager;
}
//=============================================================================
void
NelsonConfiguration::setRandomEngine(void* randomEngine)
{
    this->RandomEngine = randomEngine;
}
//=============================================================================
void*
NelsonConfiguration::getRandomEngine()
{
    return this->RandomEngine;
}
//=============================================================================
void
NelsonConfiguration::setHistoryManager(void* historyManager)
{
    this->HistoryManager = historyManager;
}
//=============================================================================
void*
NelsonConfiguration::getHistoryManager()
{
    return this->HistoryManager;
}
//=============================================================================
int
NelsonConfiguration::getNelsonEngineMode()
{
    return this->engineMode;
}
//=============================================================================
void
NelsonConfiguration::setNelsonEngineMode(int nelsonMode)
{
    this->engineMode = nelsonMode;
}
//=============================================================================
bool
NelsonConfiguration::haveEventsLoop()
{
    return ((this->engineMode != BASIC_ENGINE) && (this->engineMode != BASIC_TERMINAL)
        && (this->engineMode != WASM_TERMINAL));
}
//=============================================================================
void
NelsonConfiguration::setMaxNumCompThreads(int nbThreads)
{
    nbOfThreadsToUse = nbThreads;
}
//=============================================================================
int
NelsonConfiguration::maxNumCompThreads()
{
    return nbOfThreadsToUse;
}
//=============================================================================
void
NelsonConfiguration::setLastErrorException(size_t ID, void* lastErrorException)
{
    this->lastErrorException[ID] = lastErrorException;
}
//=============================================================================
void*
NelsonConfiguration::getLastErrorException(size_t ID)
{
    if (this->lastErrorException.count(ID) != 0) {
        return this->lastErrorException[ID];
    }
    return nullptr;
}
//=============================================================================
void
NelsonConfiguration::setLastWarningException(size_t ID, void* lastWarningException)
{
    this->lastWarningException[ID] = lastWarningException;
}
//=============================================================================
void*
NelsonConfiguration::getLastWarningException(size_t ID)
{
    if (this->lastWarningException.count(ID) != 0) {
        return this->lastWarningException[ID];
    }
    return nullptr;
}
//=============================================================================
OverloadLevelCompatibility
NelsonConfiguration::setOverloadLevelCompatibility(
    OverloadLevelCompatibility desiredOverloadLevelCompatibility)
{
    OverloadLevelCompatibility previousOverloadLevelCompatibility
        = currentOverloadLevelCompatibility;
    currentOverloadLevelCompatibility = desiredOverloadLevelCompatibility;
    return previousOverloadLevelCompatibility;
}
//=============================================================================
OverloadLevelCompatibility
NelsonConfiguration::getOverloadLevelCompatibility()
{
    return currentOverloadLevelCompatibility;
}
//=============================================================================
bool
NelsonConfiguration::isCurrentFigureOnClick()
{
    return currentFigureOnClick;
}
//=============================================================================
void
NelsonConfiguration::setCurrentFigureOnClick(bool on)
{
    currentFigureOnClick = on;
}
//=============================================================================
bool
NelsonConfiguration::isCurrentAxesOnClick()
{
    return currentAxesOnClick;
}
//=============================================================================
void
NelsonConfiguration::setCurrentAxesOnClick(bool on)
{
    currentAxesOnClick = on;
}
//=============================================================================
std::wstring
NelsonConfiguration::getDefaultFromConfFile(const std::wstring& name)
{
    std::wstring defaultsFile = getNelsonRootDirectory() + L"/etc/defaults.conf";
#ifdef _MSC_VER
    std::ifstream jsonFile(defaultsFile);
#else
    std::ifstream jsonFile(wstring_to_utf8(defaultsFile));
#endif
    std::wstring valueReturned;
    if (jsonFile.is_open()) {
        nlohmann::json data;
        try {
            data = nlohmann::json::parse(jsonFile);
            std::string _value = data[wstring_to_utf8(name)];
            valueReturned = utf8_to_wstring(_value);
        } catch (const nlohmann::json::exception&) {
            valueReturned.clear();
        }
        jsonFile.close();
    }
    return valueReturned;
}
//=============================================================================
std::wstring
NelsonConfiguration::getUpdateUrl()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    if (updateUrl.empty()) {
        updateUrl = getDefaultFromConfFile(L"update_url");
    }
    return updateUrl;
}
//=============================================================================
std::wstring
NelsonConfiguration::getWebsiteUrl()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    if (websiteUrl.empty()) {
        websiteUrl = getDefaultFromConfFile(L"website_url");
    }
    return websiteUrl;
}
//=============================================================================
std::wstring
NelsonConfiguration::getBugTrackerUrl()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    if (bugTrackerUrl.empty()) {
        bugTrackerUrl = getDefaultFromConfFile(L"issues_url");
    }
    return bugTrackerUrl;
}
//=============================================================================
void
NelsonConfiguration::setDocBookUrl(const std::wstring& url)
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    docBookUrl = url;
}
//=============================================================================
std::wstring
NelsonConfiguration::getDocBookUrl()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    if (docBookUrl.empty()) {
        std::wstring prefdir = getNelsonPreferencesDirectory();
        std::wstring docroot = prefdir + L"/docroot.json";
#ifdef _MSC_VER
        std::ifstream jsonFile(docroot);
#else
        std::ifstream jsonFile(wstring_to_utf8(docroot));
#endif
        if (jsonFile.is_open()) {
            nlohmann::json data;
            try {
                data = nlohmann::json::parse(jsonFile);
                std::string _value = data["docbook_url"];
                docBookUrl = utf8_to_wstring(_value);
            } catch (const nlohmann::json::exception&) {
                docBookUrl = getDefaultFromConfFile(L"docbook_url");
            }
            jsonFile.close();
        } else {
            docBookUrl = getDefaultFromConfFile(L"docbook_url");
        }
    }
    return docBookUrl;
}
//=============================================================================
void
NelsonConfiguration::setCurrentLocale(const std::wstring& locale)
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    currentLocale = locale;
}
//=============================================================================
std::wstring
NelsonConfiguration::getCurrentLocale()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    return currentLocale;
}
//=============================================================================
std::wstring
NelsonConfiguration::getDefaultLocale()
{
    return L"en_US";
}
//=============================================================================
static void
loadEditorUsed()
{
    std::wstring prefdir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring editorFile = prefdir + L"/default_editor.json";
#ifdef _MSC_VER
    std::ifstream jsonFile(editorFile);
#else
    std::ifstream jsonFile(wstring_to_utf8(editorFile));
#endif
    if (jsonFile.is_open()) {
        nlohmann::json data;
        std::wstring value;
        try {
            data = nlohmann::json::parse(jsonFile);
            std::string _value = data["editor"];
            value = utf8_to_wstring(_value);
        } catch (const nlohmann::json::exception&) {
            value.clear();
        }
        jsonFile.close();
        NelsonConfiguration::getInstance()->setCurrentEditor(value, false);
    }
}
//=============================================================================
bool
NelsonConfiguration::useEmbeddedEditor()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    if (!isVsCode && !embeddedEditorFlagLoaded) {
        loadEditorUsed();
        embeddedEditorFlagLoaded = true;
    }
    return useEmbeddedEditorFlag;
}
//=============================================================================
static void
saveEditorCommandLine(const std::wstring& editorCommandLine)
{
    std::wstring prefdir = NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    std::wstring editorFile = prefdir + L"/default_editor.json";
    nlohmann::json jsonObject;
    jsonObject["editor"] = wstring_to_utf8(editorCommandLine);
#ifdef _MSC_VER
    std::wofstream file(editorFile);
#else
    std::ofstream file(wstring_to_utf8(editorFile));
#endif
    if (file.is_open()) {
#ifdef _MSC_VER
        file << utf8_to_wstring(jsonObject.dump(2));
        file << L"\n";
#else
        file << jsonObject.dump(2);
        file << "\n";
#endif
        file.close();
    }
}
//=============================================================================
void
NelsonConfiguration::setVsCodeMode(bool modeOn)
{
    isVsCode = modeOn;
}
//=============================================================================
bool
NelsonConfiguration::isVsCodeMode()
{
    return isVsCode;
}
//=============================================================================
void
NelsonConfiguration::enableOpenMPParallelization(bool enable)
{
    ompEnable = enable;
}
//=============================================================================
bool
NelsonConfiguration::isOpenMPParallelizationEnabled()
{
    return ompEnable;
}
//=============================================================================
void
NelsonConfiguration::setOpenMPParallelizationThreshold(uint64_t threshold)
{
    ompThreshold = threshold;
}
//=============================================================================
uint64_t
NelsonConfiguration::getOpenMPParallelizationThreshold()
{
    return ompThreshold;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonDomainName()
{
    return NELSON_DOMAIN;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonApplicationId()
{
    return NELSON_APP_ID;
}
//=============================================================================
void
NelsonConfiguration::setCurrentEditor(const std::wstring& _editorCommandLine, bool save)
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    if (_editorCommandLine.empty()) {
        editorCommandLine = L"";
        useEmbeddedEditorFlag = true;
    } else {
        editorCommandLine = _editorCommandLine;
        useEmbeddedEditorFlag = false;
    }
    if (save) {
        saveEditorCommandLine(editorCommandLine);
    }
}
//=============================================================================
std::wstring
NelsonConfiguration::getCurrentEditor()
{
    std::lock_guard<std::recursive_mutex> lock(stringMutex);
    return editorCommandLine;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
