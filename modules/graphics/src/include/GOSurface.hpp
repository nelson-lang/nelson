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
    ArrayOf
    getCoordinateMatrix(const std::wstring& propertyName, bool isXCoord);
    void
    constructProperties() override;
    void
    setupDefaults() override;

private:
    std::vector<double> limits;
    mutable bool limitsDirty = true;

public:
    GOSurface();
    ~GOSurface() override;
    virtual std::wstring
    getType() override;
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    std::vector<double>
    getLimits() override;
};
//=============================================================================
}
//=============================================================================
