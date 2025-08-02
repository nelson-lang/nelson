//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "renameStructFieldBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "IsValidFieldname.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <algorithm>
#include <unordered_set>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::renameStructFieldBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;

    if (!argIn[0].isStruct()) {
        Error(_W("Wrong type for argument #1. struct expected."));
    }

    ArrayOf structArray = argIn[0];

    if (argIn.size() == 2) {
        // Case 1: Rename all fields
        stringVector currentNames = structArray.getFieldNames();
        stringVector newNames = argIn[1].getContentAsCStringVector();

        if (currentNames.size() != newNames.size()) {
            Error(_W("Number of field names must match number of desired names."));
        }

        for (const auto& name : newNames) {
            if (!IsValidFieldname(name)) {
                Error(_("Invalid field name: ") + name);
            }
        }

        structArray.renameFieldnames(newNames);
        retval << structArray;
        return retval;
    }

    // Case 2: Rename selected fields
    stringVector oldNames = argIn[1].getContentAsCStringVector();
    stringVector newNames = argIn[2].getContentAsCStringVector();

    if (oldNames.size() != newNames.size()) {
        Error(_W("Number of old field names must match number of new field names."));
    }

    stringVector fieldNames = structArray.getFieldNames(); // must remain same size
    std::unordered_set<std::string> existingFieldNames(fieldNames.begin(), fieldNames.end());

    // Apply valid renames only
    for (size_t i = 0; i < oldNames.size(); ++i) {
        const std::string& oldName = oldNames[i];
        const std::string& newName = newNames[i];

        // Skip if oldName doesn't exist
        auto it = std::find(fieldNames.begin(), fieldNames.end(), oldName);
        if (it == fieldNames.end()) {
            continue;
        }

        if (!IsValidFieldname(newName)) {
            Error(_("Invalid field name: ") + newName);
        }

        // Prevent overwriting another existing field
        if (oldName != newName && existingFieldNames.find(newName) != existingFieldNames.end()) {
            Error(_("Cannot rename to an existing field: ") + newName);
        }

        // Perform the rename in-place
        *it = newName;
        existingFieldNames.erase(oldName);
        existingFieldNames.insert(newName);
    }

    structArray.renameFieldnames(fieldNames);
    retval << structArray;
    return retval;
}
//=============================================================================
