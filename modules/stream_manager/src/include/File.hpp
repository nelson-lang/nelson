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
#ifdef _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include "nlsStream_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// Think to update with <boost/endian/conversion.hpp>
// http://www.boost.org/doc/libs/develop/libs/endian/doc/conversion.html
class NLSSTREAM_MANAGER_IMPEXP File
{
private:
    void* stream;
    std::wstring filename;
    std::wstring mode;
    std::wstring machineFormat;
    std::wstring encoding;
    bool bIsFilePointer;

public:
    File(bool bIsFilePointer = true);
    ~File();
    std::wstring
    getFileMode();
    void
    setFileMode(const std::wstring& _mode);
    void*
    getFilePointer();
    void
    setFilePointer(void* fp);
    std::wstring
    getFileName();
    void
    setFileName(const std::wstring& _filename);
    bool
    isInterfaceMethod();
    std::wstring
    getMachineFormat();
    void
    setMachineFormat(const std::wstring& _machineFormat);
    std::wstring
    getEncoding();
    void
    setEncoding(const std::wstring& _encoding);
};
//=============================================================================
}; // namespace Nelson
//=============================================================================
