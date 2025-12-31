//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <set>
#include <algorithm>
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include "rmfieldBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
void
unsortedRemoveDuplicates(stringVector& values)
{
    std::set<std::string> seenStrings;
    auto itr = begin(values);
    while (itr != end(values)) {
        if (seenStrings.find(*itr) != end(seenStrings)) {
            itr = values.erase(itr);
        } else {
            seenStrings.insert(*itr);
            itr++;
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::rmfieldBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    if (param1.isClassType() || param1.isHandle()) {
        OverloadRequired("rmfield");
    }
    if (param1.isStruct()) {
        stringVector namesToRemove = param2.getContentAsCStringVector();
        stringVector currentNames = param1.getFieldNames();
        stringVector common;

        unsortedRemoveDuplicates(namesToRemove);

        for (const std::string& n : namesToRemove) {
            bool have = false;
            for (const std::string& c : currentNames) {
                if (c.compare(n) == 0) {
                    have = true;
                }
            }
            if (!have) {
                Error(fmt::sprintf(_("A field named '%s' doesn't exist."), n));
            }
        }
        common = currentNames;
        for (const std::string& c : namesToRemove) {
            common.erase(std::remove(common.begin(), common.end(), c), common.end());
        }
        if (param1.isScalar()) {
            ArrayOfVector values;
            for (const std::string& c : common) {
                values.push_back(param1.getField(c));
            }
            retval << ArrayOf::structScalarConstructor(common, values);
        } else {
            Dimensions dims = param1.getDimensions();
            auto* qp = static_cast<ArrayOf*>(
                ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.getElementCount(), common, false));

            ArrayOf st = ArrayOf(NLS_STRUCT_ARRAY, dims, qp, false, common);
            for (const std::string& c : common) {
                ArrayOfVector data = param1.getFieldAsList(c);
                st.setFieldAsList(c, data);
            }
            retval << st;
        }
    } else {
        Error(_W("Wrong type for argument #1. struct expected."));
    }
    return retval;
}
//=============================================================================
