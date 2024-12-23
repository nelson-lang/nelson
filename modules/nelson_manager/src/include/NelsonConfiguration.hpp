//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include <map>
#include "nlsNelson_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum NumericFormatDisplay
{
    NLS_NUMERIC_FORMAT_SHORT = 0,
    NLS_NUMERIC_FORMAT_LONG,
    NLS_NUMERIC_FORMAT_SHORTE,
    NLS_NUMERIC_FORMAT_LONGE,
    NLS_NUMERIC_FORMAT_SHORTG,
    NLS_NUMERIC_FORMAT_LONGG,
    NLS_NUMERIC_FORMAT_SHORTENG,
    NLS_NUMERIC_FORMAT_LONGENG,
    NLS_NUMERIC_FORMAT_PLUS,
    NLS_NUMERIC_FORMAT_BANK,
    NLS_NUMERIC_FORMAT_HEX,
    NLS_NUMERIC_FORMAT_RATIONAL
};
//=============================================================================
enum LineSpacingDisplay
{
    NLS_LINE_SPACING_COMPACT,
    NLS_LINE_SPACING_LOOSE
};
//=============================================================================
enum OverloadLevelCompatibility
{
    NLS_OVERLOAD_NONE = 0,
    NLS_OVERLOAD_OBJECT_TYPES_ONLY,
    NLS_OVERLOAD_ALL_TYPES
};
//=============================================================================
class NLSNELSON_MANAGER_IMPEXP NelsonConfiguration
{
    //=============================================================================
public:
    //=============================================================================
    static NelsonConfiguration*
    getInstance();
    //=============================================================================
    bool
    getInterruptPending(size_t evaluatorID = 0);
    bool
    setInterruptPending(bool bInterruptPending, size_t evaluatorID = 0);
    //=============================================================================
    NumericFormatDisplay
    setNumericFormatDisplay(NumericFormatDisplay desiredNumericFormatDisplay);
    NumericFormatDisplay
    getNumericFormatDisplay();
    //=============================================================================
    LineSpacingDisplay
    setLineSpacingDisplay(LineSpacingDisplay desiredLineSpacingDisplay);
    LineSpacingDisplay
    getLineSpacingDisplay();
    //=============================================================================
    void
    destroy();
    //=============================================================================
    void
    enableModulesProtection();
    //=============================================================================
    void
    disableModulesProtection();
    //=============================================================================
    void
    enableFileWatcher();
    //=============================================================================
    void
    disableFileWatcher();
    //=============================================================================
    bool
    isFileWatcherEnabled();
    //=============================================================================
    bool
    isModulesProtected();
    //=============================================================================
    void
    setNelsonRootDirectory(const std::wstring& nelsonroot);
    //=============================================================================
    std::wstring
    getNelsonRootDirectory();
    //=============================================================================
    void
    setNelsonModulesDirectory(const std::wstring& directory);
    //=============================================================================
    std::wstring
    getNelsonModulesDirectory();
    //=============================================================================
    void
    setNelsonBinaryDirectory(const std::wstring& directory);
    //=============================================================================
    std::wstring
    getNelsonBinaryDirectory();
    //=============================================================================
    void
    setNelsonLibraryDirectory(const std::wstring& directory);
    //=============================================================================
    std::wstring
    getNelsonLibraryDirectory();
    //=============================================================================
    void
    setNelsonPreferencesDirectory(const std::wstring& directory);
    //=============================================================================
    std::wstring
    getNelsonPreferencesDirectory();
    //=============================================================================
    void
    setMainIOInterface(void* IOInterface);
    //=============================================================================
    void*
    getMainIOInterface();
    //=============================================================================
    void
    setMainGuiObject(void* mainGuiObject);
    //=============================================================================
    void*
    getMainGuiObject();
    //=============================================================================
    void
    setMainEvaluator(void* mainEvaluator);
    //=============================================================================
    void*
    getMainEvaluator();
    //=============================================================================
    void
    setFileManager(void* filemanager);
    //=============================================================================
    void*
    getFileManager();
    //=============================================================================
    void
    setRandomEngine(void* randomEngine);
    //=============================================================================
    void*
    getRandomEngine();
    //=============================================================================
    void
    setHistoryManager(void* historyManager);
    //=============================================================================
    void*
    getHistoryManager();
    //=============================================================================
    int
    getNelsonEngineMode();
    //=============================================================================
    void
    setNelsonEngineMode(int nelsonMode);
    //=============================================================================
    bool
    haveEventsLoop();
    //=============================================================================
    void
    setMaxNumCompThreads(int nbThreads);
    //=============================================================================
    int
    getMaxNumCompThreads();
    //=============================================================================
    void
    setLastErrorException(size_t ID, void* lastErrorException);
    void*
    getLastErrorException(size_t ID);
    //=============================================================================
    void
    setLastWarningException(size_t ID, void* lastWarningException);
    void*
    getLastWarningException(size_t ID);
    //=============================================================================
    OverloadLevelCompatibility
    setOverloadLevelCompatibility(OverloadLevelCompatibility desiredOverloadLevelCompatibility);
    OverloadLevelCompatibility
    getOverloadLevelCompatibility();
    //=============================================================================
    bool
    isCurrentFigureOnClick();
    void
    setCurrentFigureOnClick(bool on);
    //=============================================================================
    bool
    isCurrentAxesOnClick();
    void
    setCurrentAxesOnClick(bool on);
    //=============================================================================
    std::wstring
    getUpdateUrl();
    //=============================================================================
    std::wstring
    getWebsiteUrl();
    //=============================================================================
    std::wstring
    getBugTrackerUrl();
    //=============================================================================
    std::wstring
    getDocBookUrl();
    //=============================================================================
    void
    setDocBookUrl(const std::wstring& url);
    //=============================================================================
    void
    setCurrentLocale(const std::wstring& locale);
    //=============================================================================
    std::wstring
    getCurrentLocale();
    //=============================================================================
    std::wstring
    getDefaultLocale();
    //=============================================================================
private:
    NelsonConfiguration();
    //=============================================================================
    std::wstring
    getDefaultFromConfFile(const std::wstring& name);
    //=============================================================================
    static NelsonConfiguration* m_pInstance;
    //=============================================================================
    /**
     * Pending control-C
     */
    std::map<size_t, bool> InterruptPending;
    //=============================================================================
    /**
     * Current numeric format
     */
    NumericFormatDisplay currentNumericFormatDisplay;
    //=============================================================================
    /**
     * Current line spacing
     */
    LineSpacingDisplay currentLineSpacingDisplay;
    //=============================================================================
    bool modulesProtected;
    //=============================================================================
    std::wstring nelsonRootPath;
    std::wstring nelsonBinaryPath;
    std::wstring nelsonLibraryPath;
    std::wstring nelsonModulesPath;
    std::wstring nelsonPreferencesPath;
    //=============================================================================
    bool ipcEnabled;
    //=============================================================================
    bool fileWatcherEnabled;
    //=============================================================================
    void* mainEvaluator;
    void* mainInputOutputInterface;
    void* mainGuiObject;
    void* FileManager;
    void* RandomEngine;
    void* HistoryManager;
    std::map<size_t, void*> lastErrorException;
    std::map<size_t, void*> lastWarningException;
    //=============================================================================
    int engineMode;
    //=============================================================================
    int nbOfThreadsToUse;
    //=============================================================================
    OverloadLevelCompatibility currentOverloadLevelCompatibility;
    //=============================================================================
    bool currentFigureOnClick = true;
    //=============================================================================
    bool currentAxesOnClick = true;
    //=============================================================================
    std::wstring websiteUrl;
    std::wstring updateUrl;
    std::wstring bugTrackerUrl;
    std::wstring docBookUrl;
    std::wstring currentLocale;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
