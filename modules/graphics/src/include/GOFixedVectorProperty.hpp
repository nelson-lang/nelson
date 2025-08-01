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
#include "GOVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOFixedVectorProperty : public GOVectorProperty
{
    //=============================================================================
protected:
    size_t m_len;
    //=============================================================================
public:
    //=============================================================================
    GOFixedVectorProperty(size_t len) : m_len(len)
    {
        for (size_t i = 0; i < len; i++) {
            _data.push_back(0);
        }
    }
    //=============================================================================
    ~GOFixedVectorProperty() override = default;
    //=============================================================================
    void set(ArrayOf) override;
    //=============================================================================
};
//=============================================================================
};
//=============================================================================
