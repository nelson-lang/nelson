//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveClassAttribute(hid_t fid, const std::string& location, ArrayOf variableValue);
//=============================================================================
bool
h5SaveDimensionsAttribute(hid_t fid, const std::string& location, const Dimensions& dims);
//=============================================================================
bool
h5SaveEmptyAttribute(hid_t fid, const std::string& location, bool isEmpty = true);
//=============================================================================
bool
h5SaveComplexAttribute(hid_t fid, const std::string& location, bool isComplex = true);
//=============================================================================
bool
h5SaveSparseAttribute(hid_t fid, const std::string& location, bool isSparse = true);
//=============================================================================
bool
h5SaveStringAttribute(hid_t fid, const std::string& location, const std::string& attributeName,
    const std::string& content);
//=============================================================================
bool
h5SaveUint8Attribute(
    hid_t fid, const std::string& location, const std::string& attributeName, uint8 content);
//=============================================================================
bool
h5SaveUint64Attribute(
    hid_t fid, const std::string& location, const std::string& attributeName, uint64 content);
//=============================================================================
bool
h5LDeleteIfExists(hid_t fid, const std::string& location);
//=============================================================================
} // namespace Nelson
//=============================================================================
