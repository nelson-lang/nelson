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
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
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
        raiseError2(L"nelson:validators:mustBeStructAtPosition", 1);
    }

    ArrayOf structArray = argIn[0];

    if (argIn.size() == 2) {
        // Case 1: Rename all fields
        stringVector currentNames = structArray.getFieldNames();
        stringVector newNames = argIn[1].getContentAsCStringVector();

        if (currentNames.size() != newNames.size()) {
            raiseError(L"Nelson:data_structures:ERROR_NUMBER_OF_FIELD_NAMES_MUST_MATCH_NUMBER_OF_"
                       L"FIELDS_IN_NEW_STRUCT",
                ERROR_NUMBER_OF_FIELD_NAMES_MUST_MATCH_NUMBER_OF_FIELDS_IN_NEW_STRUCT);
        }

        for (const auto& name : newNames) {
            if (!IsValidFieldname(name)) {
                raiseError(L"Nelson:data_structures:ERROR_INVALID_FIELD_NAME",
                    ERROR_INVALID_FIELD_NAME, utf8_to_wstring(name));
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
        raiseError(L"Nelson:data_structures:ERROR_NUMBER_OF_OLD_FIELD_NAMES_MUST_MATCH_NUMBER_OF_"
                   L"NEW_FIELD_NAMES",
            ERROR_NUMBER_OF_OLD_FIELD_NAMES_MUST_MATCH_NUMBER_OF_NEW_FIELD_NAMES);
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
            raiseError(L"Nelson:data_structures:ERROR_INVALID_FIELD_NAME", ERROR_INVALID_FIELD_NAME,
                utf8_to_wstring(newName));
        }

        // Prevent overwriting another existing field
        if (oldName != newName && existingFieldNames.find(newName) != existingFieldNames.end()) {
            raiseError(L"Nelson:data_structures:ERROR_CANNOT_RENAME_TO_EXISTING_FIELD",
                ERROR_CANNOT_RENAME_TO_EXISTING_FIELD, utf8_to_wstring(newName));
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
