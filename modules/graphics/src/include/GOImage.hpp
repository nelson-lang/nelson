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
#include <vector>
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
class NLSGRAPHICS_IMPEXP GOImage : public GraphicsObject
{
protected:
    // CData rasterized as tightly-packed RGBA8 (row-major), Qt-free.
    std::vector<unsigned char> imgRgba;
    int imgWidth = 0;
    int imgHeight = 0;
    uint64_t imageResourceKey = 0;
    uint64_t imageRevision = 0;
    bool imageOpaque = false;
    bool
    usesUnitAlpha();
    void
    prepareTrueColorRGBA(const double* dp, indexType rows, indexType cols,
        const std::vector<double>* alpha, bool isIntegerData);
    void
    prepareTrueColorRGBA8(
        const unsigned char* dp, indexType rows, indexType cols, const std::vector<double>* alpha);
    void
    prepareMappedRGBA(const double* dp, indexType rows, indexType cols,
        const std::vector<double>* alpha, bool floatData);

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
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_IMAGE_STR;
    }

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
} // namespace Nelson
//=============================================================================
