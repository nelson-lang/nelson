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
#pragma once
//=============================================================================
#include "Stream.hpp"
#include "nlsStream_manager_exports.h"
#include <stdio.h>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSSTREAM_MANAGER_IMPEXP FileStream : public Stream
{
private:
    bool autoclose;
    FILE* fp;

public:
    FileStream(const std::wstring filename, const std::wstring accessmode);
    FileStream(FILE* afp);
    // Close the file
    ~FileStream();
    // Write a sequence of bytes to the file
    virtual void
    writeBytes(const void* data, int len);
    // Read a sequence of bytes from the file
    virtual void
    readBytes(void* data, int len);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
