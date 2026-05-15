//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LayoutHelpers.hpp"
#include "GOLayoutOptions.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
setLayoutOptions(GOAxis* ax, int64 axHandle, int tileNum, int spanRows, int spanCols,
    const std::wstring& edgeTile)
{
    GOLayoutOptions* opts = new GOLayoutOptions;
    int64 optsHandle = assignGraphicsObject(opts);
    opts->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, axHandle);

    GOGenericProperty* tileProp = opts->findProperty(GO_TILE_PROPERTY_NAME_STR, false);
    if (tileProp) {
        if (!edgeTile.empty()) {
            tileProp->set(ArrayOf::characterArrayConstructor(edgeTile));
        } else {
            tileProp->set(ArrayOf::doubleConstructor((double)tileNum));
        }
    }
    GOTwoVectorProperty* spanProp
        = dynamic_cast<GOTwoVectorProperty*>(opts->findProperty(GO_TILE_SPAN_PROPERTY_NAME_STR));
    if (spanProp) {
        spanProp->value((double)spanRows, (double)spanCols);
    }

    ax->setGoProperty(GO_LAYOUT_PROPERTY_NAME_STR, optsHandle);
}
//=============================================================================
}
//=============================================================================
