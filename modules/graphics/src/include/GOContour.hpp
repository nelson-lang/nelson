//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GraphicsObject.hpp"
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
#include "RenderInterface.hpp"
#include "ContourPoint.hpp"
#include <cstddef>
#include <vector>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOContour : public GraphicsObject
{
public:
    GOContour();
    //=============================================================================
    virtual ~GOContour();
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_CONTOUR_STR;
    }
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    void
    paintMe(RenderInterface& gc) override;
    //=============================================================================
    std::vector<double>
    getLimits() override;
    //=============================================================================
private:
    contourLineCollection contourLines;
    std::vector<double> zValues;
    // Fingerprint of the inputs the lines above were generated from. updateState()
    // skips re-tracing when nothing that shapes the contour changed. getLimits()
    // calls updateState() repeatedly during axis layout.
    std::size_t contourInputsHash = 0;
    bool contourInputsHashValid = false;
    //=============================================================================
    virtual void
    constructProperties();
    //=============================================================================
    virtual void
    setupDefaults();
    //=============================================================================
    void
    selectColorForRendering(RenderInterface& gc, double zval, double edgeAlpha);
    //=============================================================================
    ArrayOf
    generateDataArray(const std::wstring& name, bool isXcoord);
    //=============================================================================
    void
    rebuildContourMatrix();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
