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
#include <cstdint>
#include <list>
#include <vector>
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOPatch : public GraphicsObject
{
    //=============================================================================
private:
    FaceList m_faces;
    uint64_t m_geometryToken { 0 };
    virtual void
    buildPolygons(FaceList& faces);
    void
    paintMarkers(RenderInterface& gc, double lineWidth);
    bool
    synchronizeDataProperties();
    //=============================================================================
    std::vector<double> limits;
    mutable bool limitsDirty = true;
    //=============================================================================
public:
    GOPatch();
    //=============================================================================
    ~GOPatch() override;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_PATCH_STR;
    }
    //=============================================================================
    virtual void
    constructProperties();
    //=============================================================================
    virtual void
    setupDefaults();
    //=============================================================================
    void
    paintMe(RenderInterface& gc) override;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    std::vector<double>
    getLimits() override;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
