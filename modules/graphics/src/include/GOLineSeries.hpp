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
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOLineSeries : public GraphicsObject
{
public:
    virtual std::wstring
    getType() override;
    wstringVector
    getVisibleFieldnames() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_LINE_STR;
    }

    GOLineSeries();
    explicit GOLineSeries(const std::wstring& type);
    ~GOLineSeries() override;
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
    std::vector<double> limits;
    mutable bool limitsDirty = true;
    std::wstring _type;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
