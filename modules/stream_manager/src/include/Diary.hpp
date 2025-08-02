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
#include "nlsStream_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSSTREAM_MANAGER_IMPEXP Diary
{
private:
#ifdef _MSC_VER
#pragma warning(disable : 4251)
#endif
    std::wstring diaryFilename;
    bool bState;

public:
    Diary();
    ~Diary();
    bool
    SetFilename(const std::wstring& wFilename);
    std::wstring
    getFilename();
    void
    writeMessage(const std::string& msg);
    void
    writeMessage(const std::wstring& msg);
    bool
    getState();
    bool
    setState(bool bNewState);
    void
    toggle();
};
//=============================================================================
} // namespace Nelson
  //===========================================
