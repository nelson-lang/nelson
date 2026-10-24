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
#include "nlsGraphics_exports.h"
#include "GORestrictedStringColorProperty.hpp"
#include "GORestrictedStringScalarProperty.hpp"
#include "GOImage.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOSurface : public GOImage
{
    void
    autoXMode();
    void
    autoYMode();
    void
    autoCMode();
    std::vector<std::vector<coloredPoint>>
    buildQuadsNoTexMap(GORestrictedStringColorProperty* cp, GORestrictedStringScalarProperty* ap);
    std::vector<std::vector<coloredPoint>>
    buildTexturedQuads(bool bakeColor);
    void
    sampleTexel(double u, double v, double& r, double& g, double& b) const;
    RGBAImageView
    textureView() const;
    ArrayOf
    getCoordinateMatrix(const std::wstring& propertyName, bool isXCoord);
    void
    constructProperties() override;
    void
    setupDefaults() override;

private:
    std::vector<double> limits;
    std::vector<std::vector<coloredPoint>> m_surfaceQuads;
    std::vector<std::vector<coloredPoint>> m_edgeQuads;
    mutable bool limitsDirty = true;
    bool m_quadsDirty = true;
    uint64_t m_geometryToken { 0 };
    // Whether the cached quads were tessellated with vertex normals for a
    // visible light: a flip in light presence forces a rebuild, the mere
    // existence of a light does not.
    bool m_quadsBuiltWithLights = false;
    // Texture mode the cached quads were built for: 0 = not textured, 1 = crisp
    // (white + UV, for a texture-sampling backend), 2 = baked (texel per vertex,
    // for a backend that cannot sample a bound texture). The same surface is
    // painted by several renderers that need different builds, so a mismatch
    // forces a rebuild (see paintMe).
    int m_quadsTextureMode = 0;
    // CLim actually used to rasterize the color texture. The axis auto-CLim is
    // pushed with setTwoVectorDefault, which does not raise the property changed
    // flag, so a surface that shares an axis with a later child (meshc/surfc add
    // a contour that widens CLim) would otherwise keep a color image built with
    // the narrower CLim. Remembering it lets updateState rebuild on a mismatch.
    double m_lastClimLo { 0.0 };
    double m_lastClimHi { 0.0 };
    bool m_lastClimValid { false };

public:
    GOSurface();
    ~GOSurface() override;
    virtual std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_SURFACE_STR;
    }
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    std::vector<double>
    getLimits() override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
