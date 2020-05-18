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
#pragma once
//=============================================================================
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define TYPE_PROPERTY_STR "Type"
#define TAG_PROPERTY_STR "Tag"
#define VISIBLE_PROPERTY_STR "Visible"
#define NUMBER_PROPERTY_STR "Number"
#define USERDATA_PROPERTY_STR "UserData"
#define SCREENDEPTH_PROPERTY_STR "ScreenDepth"
#define POSITION_PROPERTY_STR "Position"
#define POINTERLOCATION_PROPERTY_STR "PointerLocation"
#define SCREENSIZE_PROPERTY_STR "ScreenSize"
#define PARENT_PROPERTY_STR "Parent"
#define CURRENTFIGURE_PROPERTY_STR "CurrentFigure"
#define CHILDREN_PROPERTY_STR "Children"
#define NAME_PROPERTY_STR "Name"
//=============================================================================
class GOProperty
{
private:
    bool wasModified;
    bool writeProtected;

public:
    //=============================================================================
    GOProperty()
    {
        wasModified = false;
        writeProtected = false;
    }
    //=============================================================================
    virtual ~GOProperty() = default;
    //=============================================================================
    virtual ArrayOf
    get()
        = 0;
    //=============================================================================
    virtual void set(ArrayOf /*unused*/) { wasModified = true; }
    //=============================================================================
    virtual std::string
    print(const std::string& propertyName)
        = 0;
    //=============================================================================
    void
    forceWriteProtected()
    {
        writeProtected = true;
    }
    //=============================================================================
    bool
    isWriteProtected()
    {
        return writeProtected;
    }
    //=============================================================================
    void
    clearModified()
    {
        wasModified = false;
    }
    //=============================================================================
    bool
    isModified()
    {
        return wasModified;
    }
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
