//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsWarningStruct.hpp"
#include "NelsonException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsWarningStruct(const ArrayOf& arg, wstringVector& identifiers, wstringVector& states)
{
    identifiers.clear();
    states.clear();

    if (!arg.isStruct()) {
        return false;
    }
    stringVector fs = arg.fieldNames();
    if (fs.size() != 2) {
        return false;
    }
    if (fs[0] != "identifier") {
        return false;
    }
    if (fs[1] != "state") {
        return false;
    }
    ArrayOfVector idArray = arg.fieldAsList("identifier");
    ArrayOfVector stateArray = arg.fieldAsList("state");

    for (auto& k : idArray) {
        try {
            identifiers.push_back(k.asWideString());
        } catch (const NelsonException&) {
            return false;
        }
    }
    for (auto& k : stateArray) {
        try {
            states.push_back(k.asWideString());
        } catch (const NelsonException&) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
