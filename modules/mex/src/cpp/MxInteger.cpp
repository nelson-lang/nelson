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
#include "MxInteger.h"
//=============================================================================
bool
mxIsInt8(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxINT8_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsInt16(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxINT16_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsInt32(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxINT32_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsInt64(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxINT64_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsUint8(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxUINT8_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsUint16(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxUINT16_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsUint32(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxUINT32_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsUint64(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxUINT64_CLASS;
    }
    return false;
}
//=============================================================================
