//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
