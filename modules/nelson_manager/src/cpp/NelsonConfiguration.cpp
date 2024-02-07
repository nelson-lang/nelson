//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <new>
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NelsonConfiguration* NelsonConfiguration::m_pInstance = nullptr;
//=============================================================================
NelsonConfiguration::NelsonConfiguration()
{
    InterruptPending.clear();
    currentOverloadLevelCompatibility = NLS_OVERLOAD_ALL_TYPES;
    currentNumericFormatDisplay = NLS_NUMERIC_FORMAT_SHORT;
    currentLineSpacingDisplay = NLS_LINE_SPACING_LOOSE;
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
    if (InterruptPending.find(evaluatorID) == InterruptPending.end()) {
        return false;
    }
    return InterruptPending[evaluatorID];
}
//=============================================================================
bool
NelsonConfiguration::setInterruptPending(bool bInterruptPending, size_t evaluatorID)
{
    bool bPrevious = getInterruptPending(evaluatorID);
    InterruptPending[evaluatorID] = bInterruptPending;
    return bPrevious;
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
    nelsonRootPath = nelsonroot;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonRootDirectory()
{
    return nelsonRootPath;
}
//=============================================================================
void
NelsonConfiguration::setNelsonBinaryDirectory(const std::wstring& directory)
{
    nelsonBinaryPath = directory;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonBinaryDirectory()
{
    return nelsonBinaryPath;
}
//=============================================================================
void
NelsonConfiguration::setNelsonLibraryDirectory(const std::wstring& directory)
{
    nelsonLibraryPath = directory;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonLibraryDirectory()
{
    return nelsonLibraryPath;
}
//=============================================================================
void
NelsonConfiguration::setNelsonModulesDirectory(const std::wstring& directory)
{
    nelsonModulesPath = directory;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonModulesDirectory()
{
    return nelsonModulesPath;
}
//=============================================================================
void
NelsonConfiguration::setNelsonPreferencesDirectory(const std::wstring& directory)
{
    nelsonPreferencesPath = directory;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonPreferencesDirectory()
{
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
    return ((this->engineMode != BASIC_ENGINE) && (this->engineMode != BASIC_TERMINAL));
}
//=============================================================================
void
NelsonConfiguration::setMaxNumCompThreads(int nbThreads)
{
    nbOfThreadsToUse = nbThreads;
}
//=============================================================================
int
NelsonConfiguration::getMaxNumCompThreads()
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

} // namespace Nelson
//=============================================================================
