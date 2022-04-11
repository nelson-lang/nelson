//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicObjectGet.hpp"
#include "Error.hpp"
#include "GOProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
graphicObjectGet(GraphicObject* ptr, const std::string& propertyName)
{
    ArrayOf value;
    if (ptr != nullptr) {
        ptr->refreshProperties();
        GOProperty* property = ptr->searchProperty(propertyName);
        if (property != nullptr) {
            value = property->get();
        } else {
            Error(_W("Valid property expected."));
        }
    } else {
        Error(_W("Valid graphic_object expected."));
    }
    return value;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
