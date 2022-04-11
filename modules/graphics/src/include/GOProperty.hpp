//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
#define OUTERPOSITION_PROPERTY_STR "OuterPosition"
#define INNERPOSITION_PROPERTY_STR "InnerPosition"
#define POSITION_PROPERTY_STR "Position"
#define POINTERLOCATION_PROPERTY_STR "PointerLocation"
#define SCREENSIZE_PROPERTY_STR "ScreenSize"
#define PARENT_PROPERTY_STR "Parent"
#define CURRENTFIGURE_PROPERTY_STR "CurrentFigure"
#define CHILDREN_PROPERTY_STR "Children"
#define NAME_PROPERTY_STR "Name"
#define COLOR_PROPERTY_STR "Color"
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
