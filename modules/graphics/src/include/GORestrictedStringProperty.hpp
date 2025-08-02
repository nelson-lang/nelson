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
#include "GOStringProperty.hpp"
#include "GORestrictedStringProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GORestrictedStringProperty : public GOStringProperty
{
protected:
    std::vector<std::wstring> m_dictionary;

public:
    GORestrictedStringProperty(const std::vector<std::wstring>& dict) : m_dictionary(dict)
    {
        _data = dict[0];
    }
    GORestrictedStringProperty(const wchar_t** dict);
    void set(ArrayOf) override;
};
//=============================================================================
};
//=============================================================================
