//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#define H5_BUILT_AS_DYNAMIC_LIB
#include <hdf5.h>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "HDF5_helpers.hpp"
#include "Exception.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void* hdf5ErrorFunction;
static H5E_auto_t oef;
//=============================================================================
void
disableHdf5Warning()
{
    H5Eget_auto(H5E_DEFAULT, &oef, &hdf5ErrorFunction);
    H5Eset_auto(H5E_DEFAULT, 0, 0);
}
//=============================================================================
void
enableHdf5Warning()
{
    H5Eset_auto(H5E_DEFAULT, oef, hdf5ErrorFunction);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
