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
//=============================================================================
#include <hdf5.h>
#include "ArrayOf.hpp"
//=============================================================================
#define NELSON_SCHEMA 1
#define NELSON_HEADER L"Nelson 1.0 NH5-file"
#define NELSON_HEADER_VERSION 0x0001
#define NELSON_HEADER_ENDIAN 0x4d49
#define NELSON_SCHEMA_STR "NELSON_schema"
#define NELSON_CLASS_STR "NELSON_class"
#define NELSON_EMPTY_STR "NELSON_empty"
#define NELSON_COMPLEX_STR "NELSON_complex"
#define NELSON_SPARSE_STR "NELSON_sparse"
#define NELSON_SPARSE_NZMAX_STR "NELSON_nzmax"
#define NELSON_DIMENSIONS_STR "NELSON_dimensions"
#define NELSON_OBJECT_STR "NELSON_object"
#define FIELDNAMES_STR "fieldnames"
#define NELSON_COMPRESSION_LEVEL 3
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isNelsonH5File(hid_t fid);
//=============================================================================
int32
getNelsonH5Schema(hid_t fid);
//=============================================================================
bool
addSchemaFormat(hid_t obj_id);
//=============================================================================
bool
updateNelsonH5Header(hid_t fid);
//=============================================================================
stringVector
getVariableNames(hid_t fid);
//=============================================================================
std::string
getNelsonClass(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
Dimensions
getNelsonDimensions(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
bool
isNelsonEmpty(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
bool
isNelsonSparse(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
bool
isNelsonObject(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
bool
isNelsonComplex(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
uint64
getNelsonNzmax(hid_t fid, const std::string& location, const std::string& variableName);
//=============================================================================
hid_t
setCompression(Dimensions dims, bool useCompression);
//=============================================================================
} // namespace Nelson
//=============================================================================
