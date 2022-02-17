//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <set>
#include <algorithm>
#include <fmt/printf.h>
#include <fmt/format.h>
#include "rmfieldBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "characters_encoding.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
void
unsortedRemoveDuplicates(stringVector& values)
{
    std::set<std::string> seenStrings;
    auto itr = begin(values);
    while (itr != end(values)) {
        if (seenStrings.find(*itr) != end(seenStrings))
            itr = values.erase(itr);
        else {
            seenStrings.insert(*itr);
            itr++;
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::rmfieldBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "rmfield", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        if (param1.isClassStruct() || param1.isHandle()) {
            retval = OverloadFunction(eval, nLhs, argIn, "rmfield", bSuccess);
            if (bSuccess) {
                return retval;
            }
            OverloadRequired(eval, argIn, Overload::OverloadClass::UNARY, "rmfield");
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
                auto* qp = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
                    NLS_STRUCT_ARRAY, dims.getElementCount(), common, false));

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
    }
    return retval;
}
//=============================================================================
