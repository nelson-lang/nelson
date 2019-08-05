//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    this->mode = std::move(_mode);
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
    this->filename = std::move(_filename);
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
