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
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOScatterSeries : public GraphicsObject
{
public:
    virtual std::wstring
    getType() override;

    GOScatterSeries();
    ~GOScatterSeries() override;
    virtual void
    constructProperties();
    virtual void
    setupDefaults();
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    std::vector<double>
    getLimits() override;

private:
    mutable bool cDataDirty = true;
    mutable double cachedMinC = 0.0;
    mutable double cachedMaxC = 1.0;

    std::vector<double> limits;
    mutable bool limitsDirty = true;
};
//=============================================================================
}
//=============================================================================
