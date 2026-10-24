//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include <cstdint>
#include <atomic>
#include <mutex>
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
    // Profiler on/off state, published by the profiler module so other modules
    // (e.g. the web desktop) can read it without linking nlsProfiler.
    bool
    isProfilerActive();
    void
    setProfilerActive(bool active);
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
    bool
    setTruncateMatricesDisplay(bool truncateMatrices);
    bool
    getTruncateMatricesDisplay();
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
    // Figures render through the headless RenderWeb host instead of Qt. Inlined so
    // the hot path (createFigureHost) is a relaxed atomic load, not a cross-DLL
    // call. Covers only the opt-in `--webview` case; WEB_GUI and WASM_TERMINAL
    // come from the engine mode, and the graphics helper ORs the two.
    inline void
    setWebGraphicsMode(bool on)
    {
        m_webGraphicsMode.store(on, std::memory_order_relaxed);
    }
    inline bool
    isWebGraphicsMode() const
    {
        return m_webGraphicsMode.load(std::memory_order_relaxed);
    }
    //=============================================================================
    bool
    haveEventsLoop();
    //=============================================================================
    // graphics_qt updates the count as Qt figure windows come and go. The
    // interpreter reads hasOpenFigureWindows() at every statement boundary to
    // pump the GUI event loop while a script runs. Inlined to avoid a cross-DLL
    // call on the hot path; 0 in headless runs.
    inline void
    incrementOpenFigureWindowCount()
    {
        m_openFigureWindowCount.fetch_add(1, std::memory_order_relaxed);
    }
    inline void
    decrementOpenFigureWindowCount()
    {
        m_openFigureWindowCount.fetch_sub(1, std::memory_order_relaxed);
    }
    inline bool
    hasOpenFigureWindows() const
    {
        return m_openFigureWindowCount.load(std::memory_order_relaxed) > 0;
    }
    //=============================================================================
    void
    setMaxNumCompThreads(int nbThreads);
    //=============================================================================
    int
    maxNumCompThreads();
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
    bool
    useEmbeddedEditor();
    //=============================================================================
    void
    setCurrentEditor(const std::wstring& _editorCommandLine, bool save);
    //=============================================================================
    std::wstring
    getCurrentEditor();
    //=============================================================================
    void
    setVsCodeMode(bool modeOn);
    //=============================================================================
    bool
    isVsCodeMode();
    //=============================================================================
    void
    enableOpenMPParallelization(bool enable);
    //=============================================================================
    bool
    isOpenMPParallelizationEnabled();
    //=============================================================================
    void
    setOpenMPParallelizationThreshold(uint64_t threshold);
    //=============================================================================
    uint64_t
    getOpenMPParallelizationThreshold();
    //=============================================================================
    std::wstring
    getNelsonDomainName();
    //=============================================================================
    std::wstring
    getNelsonApplicationId();
    //=============================================================================
    bool
    isClosing() const
    {
        return m_isClosing;
    }
    //=============================================================================
    void
    setAsClosing(bool closing)
    {
        m_isClosing = closing;
    }
    //=============================================================================
    void
    setDarkMode(bool darkMode)
    {
        m_isDarkMode = darkMode;
    }
    //=============================================================================
    // Set once the quit is final (no CANCEL_QUIT possible), before the finish
    // script unloads module gateways.
    bool
    isShuttingDown() const
    {
        return m_isShuttingDown;
    }
    //=============================================================================
    void
    setAsShuttingDown(bool shuttingDown)
    {
        m_isShuttingDown = shuttingDown;
    }
    //=============================================================================
    bool
    isDarkMode() const
    {
        return m_isDarkMode;
    }
    //=============================================================================
private:
    NelsonConfiguration();
    //=============================================================================
    std::wstring
    getDefaultFromConfFile(const std::wstring& name);
    //=============================================================================
    static NelsonConfiguration* m_pInstance;
    //=============================================================================
    std::map<size_t, bool> InterruptPending;
    std::mutex InterruptPendingMutex;
    std::atomic_bool mainInterruptPending { false };
    std::atomic_bool m_profilerActive { false };
    // On-screen (non-headless) figure windows alive; see the inline accessors.
    std::atomic<long> m_openFigureWindowCount { 0 };
    //=============================================================================
    NumericFormatDisplay currentNumericFormatDisplay;
    //=============================================================================
    LineSpacingDisplay currentLineSpacingDisplay;
    bool currentTruncateMatricesDisplay;
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
    bool useEmbeddedEditorFlag = true;
    bool embeddedEditorFlagLoaded = false;
    bool isVsCode = false;
    //=============================================================================
    std::wstring websiteUrl;
    std::wstring updateUrl;
    std::wstring bugTrackerUrl;
    std::wstring docBookUrl;
    std::wstring currentLocale;
    std::wstring editorCommandLine;
    //=============================================================================
    uint64_t ompThreshold;
    bool ompEnable;
    //=============================================================================
    bool m_isClosing = false;
    //=============================================================================
    bool m_isDarkMode = false;
    //=============================================================================
    bool m_isShuttingDown = false;
    //=============================================================================
    // Kept last on purpose: the offsets of the existing members must not change,
    // because inline accessors are compiled into other DLLs.
    std::atomic_bool m_webGraphicsMode { false };
    //=============================================================================
    mutable std::recursive_mutex stringMutex;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
