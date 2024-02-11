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
#include <vector>
#include "ArrayOf.hpp"
#include "GOGenericProperty.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOVectorProperty : public GOGenericProperty
{
private:
    bool _finiteOnly = false;

protected:
    std::vector<double> _data;

public:
    GOVectorProperty() = default;
    GOVectorProperty(bool finiteOnly);
    ~GOVectorProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    double&
    operator[](int ndx);
    double&
    at(int ndx);
    std::vector<double>
    data();
    void
    data(const std::vector<double>& m);
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
