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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOArrayOfProperty : public GOGenericProperty
{
protected:
    ArrayOf _data;

public:
    GOArrayOfProperty() : _data(ArrayOf::emptyConstructor()) { }
    ~GOArrayOfProperty() override = default;
    ArrayOf
    get() override;
    void
    set(ArrayOf m) override;
    ArrayOf
    data();
    void
    data(const ArrayOf& m);
    std::wstring
    toWideString() override;
};
//=============================================================================
};
//=============================================================================
