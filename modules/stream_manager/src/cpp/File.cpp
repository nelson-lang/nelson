//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    this->filename = L"";
    this->mode = L"";
}
//=============================================================================
File::~File()
{
    if (this->bIsFilePointer) {
        if (this->stream) {
            FILE* fp = (FILE*)this->stream;
            fclose(fp);
        }
    }
    this->stream = nullptr;
    this->filename = L"";
    this->mode = L"";
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
File::setFileMode(std::wstring _mode)
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
File::setFileName(std::wstring _filename)
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
}
//=============================================================================
