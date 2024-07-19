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
#include "GOGenericProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOStringVectorProperty : public GOGenericProperty
{
protected:
    std::vector<std::wstring> _data;

public:
    GOStringVectorProperty() = default;
    ~GOStringVectorProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    std::vector<std::wstring>
    data();
    void
    data(const std::wstring& m);
    void
    data(const std::vector<std::wstring>& m);
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
