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
#include <string>
#include "nlsGraphics_exports.h"
#include "GOAxis.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Polar axes object (class 'polaraxes'). Reuses GOAxis in polar mode and adds the
// polar properties (ThetaDir, ThetaZeroLocation, RColor, ThetaColor,
// RAxisLocation, grid/tick controls, ...). getType() stays the inherited "axes"
// so isType() behaves as for polar-mode axes; the Type property reads
// "polaraxes" (set in GOAxis::updateState) and getClassName() reports the polar
// class.
class NLSGRAPHICS_IMPEXP GOPolarAxes : public GOAxis
{
public:
    //=============================================================================
    GOPolarAxes();
    //=============================================================================
    ~GOPolarAxes() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
private:
    //=============================================================================
    void
    ensurePolarRuler(const wchar_t* propName);
    //=============================================================================
    void
    constructPolarProperties();
    //=============================================================================
    void
    setupPolarDefaults();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
