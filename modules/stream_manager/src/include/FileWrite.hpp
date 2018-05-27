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
#include "ArrayOf.hpp"
#include "Endian.hpp"
#include "Evaluator.hpp"
#include "File.hpp"
#include "nlsStream_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
typedef enum
{
    FWRITE_NO_ERROR,
    FWRITE_DATA_TYPE_NOT_SUPPORTED,
    FWRITE_FILE_DESTINATION_NOT_SUPPORTED,
    FWRITE_ALLOCATION_MEMORY,
    FWRITE_INVALID_FILE,
    FWRITE_ENDIAN_CONVERSION_NOT_SUPPORTED
} FWRITE_ERROR_TYPE;

NLSSTREAM_MANAGER_IMPEXP FWRITE_ERROR_TYPE
FileWrite(Evaluator* eval, File* fp, ArrayOf src, Class destClass, size_t skip,
    bool bIsLittleEndian, int& sizeWritten);
}; // namespace Nelson
//=============================================================================
