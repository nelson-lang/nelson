//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "permuteBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Permute.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::permuteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param2 = argIn[1];
    std::vector<indexType> permutationVector = param2.getContentAsIndexVector();

    if (permutationVector.size() < argIn[0].getDimensions().getLength()) {
        Error(_W("ORDER must have at least N elements for an N-D array."));
    }
    const auto [minPermutationIndex, maxPermutationIndex]
        = std::minmax_element(begin(permutationVector), end(permutationVector));
    if ((*maxPermutationIndex != permutationVector.size()) || (*minPermutationIndex != 1)) {
        Error(_W("Second argument is not a valid permutation."));
    }
    bool needToOverload = false;
    ArrayOf res = Permute(argIn[0], permutationVector, needToOverload);
    if (needToOverload) {
        OverloadRequired("permute");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
