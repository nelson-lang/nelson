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
#include "GOGenericProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOStringProperty : public GOGenericProperty
{
protected:
    std::wstring _data;

public:
    GOStringProperty() = default;
    ~GOStringProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    std::wstring
    data();
    void
    data(const std::wstring& m);
    bool
    isEqual(const std::wstring& m);
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
