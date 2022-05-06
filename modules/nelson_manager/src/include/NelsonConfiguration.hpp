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
#include "nlsNelson_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum NumericFormatDisplay
{
    NLS_NUMERIC_FORMAT_SHORT,
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
class NLSNELSON_MANAGER_IMPEXP NelsonConfiguration
{
    //=============================================================================
public:
    //=============================================================================
    static NelsonConfiguration*
    getInstance();
    //=============================================================================
    bool
    getInterruptPending();
    bool
    setInterruptPending(bool bInterruptPending);
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
    setMainGuiObject(void* mainGuiObject);
    //=============================================================================
    void*
    getMainGuiObject();
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
private:
    NelsonConfiguration();
    //=============================================================================
    static NelsonConfiguration* m_pInstance;
    //=============================================================================
    /**
     * Pending control-C
     */
    bool InterruptPending;
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
    //=============================================================================
    bool ipcEnabled;
    //=============================================================================
    void* mainGuiObject;
    void* FileManager;
    void* RandomEngine;
    void* HistoryManager;
    //=============================================================================
    int engineMode;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
