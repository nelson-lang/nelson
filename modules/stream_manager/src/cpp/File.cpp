//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "File.hpp"
//=============================================================================
#ifdef _MSC_VER
#define fileno _fileno
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
File::File(bool bIsFilePointer)
{
    this->bIsFilePointer = bIsFilePointer;
    this->stream = nullptr;
    this->filename.clear();
    this->mode.clear();
}
//=============================================================================
File::~File()
{
    if (this->bIsFilePointer) {
        if (this->stream) {
            FILE* fp = static_cast<FILE*>(this->stream);
            fclose(fp);
        }
    }
    this->stream = nullptr;
    this->filename.clear();
    this->mode.clear();
    this->bIsFilePointer = false;
}
//=============================================================================
std::wstring
File::getFileMode()
{
    return this->mode;
}
//=============================================================================
void
File::setFileMode(const std::wstring& _mode)
{
    this->mode = _mode;
}
//=============================================================================
void*
File::getFilePointer()
{
    return this->stream;
}
//=============================================================================
void
File::setFilePointer(void* fp)
{
    this->stream = fp;
}
//=============================================================================
std::wstring
File::getFileName()
{
    return this->filename;
}
//=============================================================================
void
File::setFileName(const std::wstring& _filename)
{
    this->filename = _filename;
}
//=============================================================================
bool
File::isInterfaceMethod()
{
    return !this->bIsFilePointer;
}
//=============================================================================
std::wstring
File::getMachineFormat()
{
    return this->machineFormat;
}
//=============================================================================
void
File::setMachineFormat(const std::wstring& _machineFormat)
{
    this->machineFormat = _machineFormat;
}
//=============================================================================
std::wstring
File::getEncoding()
{
    return this->encoding;
}
//=============================================================================
void
File::setEncoding(const std::wstring& _encoding)
{
    this->encoding = _encoding;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
