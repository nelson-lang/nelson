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
#include <QtGui/QImage>
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
class NLSGRAPHICS_IMPEXP GOImage : public GraphicsObject
{
protected:
    QImage img;
    void
    prepareImageRGBNoAlphaMap(const double* dp, indexType rows, indexType cols,
        std::vector<double>& alpha, bool isIntegerData);
    double*
    RGBExpandImage(const double* dp, indexType rows, indexType cols, bool floatData);

    std::vector<double>
    getAlphaMap(indexType rows, indexType cols);

    virtual void
    constructProperties();
    virtual void
    setupDefaults();

private:
    std::vector<double> limits;
    mutable bool limitsDirty = true;

public:
    virtual std::wstring
    getType() override;

    GOImage();
    ~GOImage() override;
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    std::vector<double>
    getLimits() override;
    void
    updateCAlphadata();
};
//=============================================================================
}
//=============================================================================
