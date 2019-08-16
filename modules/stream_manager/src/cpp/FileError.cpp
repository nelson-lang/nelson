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
#include "FileError.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
FileError(FilesManager* fm, int no, bool withClear, int& errorCode, std::string& errorMessage)
{
    errorMessage.clear();
    if (fm) {
        if (fm->isStdStream(no)) {
            errorCode = 0;
            errorMessage = "";
            return true;
        }
        FILE* fptr = static_cast<FILE*>(fm->getFilePointer(no));
        if (fptr) {
            errorCode = -(ferror(fptr));
            if (errorCode != 0) {
                errorMessage = std::string(strerror(errorCode));
            }
            if (withClear) {
                clearerr(fptr);
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
