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
#include "EmptyHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Dimensions
emptyDimensionsHelper(Dimensions& dims, indexType dim)
{
    Dimensions dimsOut(dims);
    if (dims.getLength() == 2) {
        switch (dim) {
        case 0: {
            dimsOut[0] = 1;
            if (dims[0] == 0 && dims[1] == 0) {
                dimsOut[1] = 1;
            } else {
                if (dims[0] == 1) {
                    dimsOut[1] = 1;
                }
            }
        } break;
        case 1: {
            dimsOut[0] = 1;
            if (dims[0] == 0 && dims[1] == 0) {
                dimsOut[1] = 0;
            }
        } break;
        case 2: {
            if (dims[0] == 0 && dims[1] == 0) {
                dimsOut[0] = 0;
            } else {
                dimsOut[0] = dims.getAt(0);
            }
            dimsOut[1] = 1;
        } break;
        default: {
            if (dims[0] == 0 && dims[1] == 0) {
                dimsOut[0] = 0;
                dimsOut[1] = 0;
            }
        } break;
        }
        return dimsOut;
    }
    int d = dim == 0 ? -1 : (int)dim;
    if (d < 0) {
        for (indexType i = 0; i < dims.getLength(); i++) {
            if (dims.getDimensionLength(i) != 1) {
                dim = i;
                break;
            }
        }
    }
    if (dim < dims.getLength()) {
        dimsOut[dim] = 1;
    }
    return dimsOut;
}
//=============================================================================
}
//=============================================================================
