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
#include "GOStringVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GORestrictedStringVectorProperty : public GOStringVector
{
private:
    std::vector<std::wstring> m_dictionary;

public:
    GORestrictedStringVectorProperty(const wchar_t** dict);
    ~GORestrictedStringVectorProperty() override = default;
    void set(ArrayOf) override;
};
//=============================================================================
};
//=============================================================================
