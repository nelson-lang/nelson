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
#include "h5SaveCell.hpp"
#include "h5SaveHelpers.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5SaveVariable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
h5SaveCell(hid_t fid, const std::string& location, const std::string& variableName,
    ArrayOf VariableValue, bool useCompression)
{
    bool bSuccess = false;
    std::string h5path;
    if (location == "/") {
        h5path = location + variableName;
    } else {
        h5path = location + "/" + variableName;
    }

    h5LDeleteIfExists(fid, h5path);

    hid_t gcpl = H5Pcreate(H5P_GROUP_CREATE);
    hid_t group = H5Gcreate(fid, h5path.c_str(), H5P_DEFAULT, gcpl, H5P_DEFAULT);
    herr_t status = H5Gclose(group);
    if (status < 0) {
        return false;
    }
    Dimensions dims = VariableValue.getDimensions();
    auto* elements = (ArrayOf*)VariableValue.getDataPointer();
    for (indexType k = 0; k < dims.getElementCount(); k++) {
        ArrayOf element = elements[k];
        std::string name = std::to_string(k);
        bSuccess = h5SaveVariable(fid, h5path + std::string("/"), name, element, useCompression);
        if (!bSuccess) {
            return false;
        }
    }
    bSuccess = h5SaveClassAttribute(fid, h5path, VariableValue);
    if (!bSuccess) {
        return false;
    }
    bSuccess = h5SaveDimensionsAttribute(fid, h5path, dims);
    if (!bSuccess) {
        return false;
    }
    if (dims.isEmpty(false)) {
        bSuccess = h5SaveEmptyAttribute(fid, h5path);
    }
    return bSuccess;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
