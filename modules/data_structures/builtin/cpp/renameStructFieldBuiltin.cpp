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
#include "ErrorID.hpp"
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
        Error(L"Nelson:data_structures:wrongTypeArgument1Struct",
            L"Wrong type for argument #%d: %s expected.", 1, L"struct");
    }

    ArrayOf structArray = argIn[0];

    if (argIn.size() == 2) {
        stringVector currentNames = structArray.fieldNames();
        stringVector newNames = argIn[1].asUtf8StringVector();

        if (currentNames.size() != newNames.size()) {
            Error(L"Nelson:data_structures:fieldNamesMatchNewNames",
                L"Number of field names must match number of desired names.");
        }

        for (const auto& name : newNames) {
            if (!IsValidFieldname(name)) {
                Error("Nelson:renameStructField:invalidFieldName", "Invalid field name: %s", name);
            }
        }

        structArray.renameFieldnames(newNames);
        retval << structArray;
        return retval;
    }

    stringVector oldNames = argIn[1].asUtf8StringVector();
    stringVector newNames = argIn[2].asUtf8StringVector();

    if (oldNames.size() != newNames.size()) {
        Error(L"Nelson:data_structures:numberOldFieldNamesMatch",
            L"Number of old field names must match number of new field names.");
    }

    stringVector fieldNames = structArray.fieldNames(); // must remain same size
    std::unordered_set<std::string> existingFieldNames(fieldNames.begin(), fieldNames.end());

    for (size_t i = 0; i < oldNames.size(); ++i) {
        const std::string& oldName = oldNames[i];
        const std::string& newName = newNames[i];

        auto it = std::find(fieldNames.begin(), fieldNames.end(), oldName);
        if (it == fieldNames.end()) {
            continue;
        }

        if (!IsValidFieldname(newName)) {
            Error("Nelson:renameStructField:invalidFieldName", "Invalid field name: %s", newName);
        }

        if (oldName != newName && existingFieldNames.find(newName) != existingFieldNames.end()) {
            Error("Nelson:renameStructField:fieldExists", "Cannot rename to an existing field: %s",
                newName);
        }

        *it = newName;
        existingFieldNames.erase(oldName);
        existingFieldNames.insert(newName);
    }

    structArray.renameFieldnames(fieldNames);
    retval << structArray;
    return retval;
}
//=============================================================================
