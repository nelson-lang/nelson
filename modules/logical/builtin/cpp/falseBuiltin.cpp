//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include "falseBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "LogicalConstructors.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LogicalGateway::falseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    bool bIsSparse = false;
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        Dimensions dim(1, 1);
        retval << FalseConstructor(dim, bIsSparse);
    } else {
        Dimensions dim;
        indexType idxMax = argIn.size();
        if ((static_cast<double>(argIn.size()) - 2.) >= 0.) {
            indexType pos = argIn.size() - 2;
            if (argIn[pos].isRowVectorCharacterArray() || argIn[pos].isScalarStringArray()) {
                std::wstring arg = argIn[pos].getContentAsWideString();
                if (arg.compare(L"like") == 0) {
                    ArrayOf arg = argIn[pos + 1];
                    if (arg.getDataClass() != NLS_LOGICAL) {
                        Error(_W("Input following \'like\' is not a logical array."));
                    }
                    bIsSparse = arg.isSparse();
                    idxMax = argIn.size() - 2;
                    if (idxMax == 0) {
                        dim[0] = 1;
                        dim[1] = 1;
                    }
                } else {
                    Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_VALUE, pos + 1));
                }
            }
        }
        for (indexType k = 0; k < idxMax; k++) {
            ArrayOf arg = argIn[k];
            indexType idx = arg.getContentAsScalarIndex();
            dim[k] = idx;
        }
        if (idxMax == 1) {
            dim[1] = dim[0];
        }
        dim.simplify();
        retval << FalseConstructor(dim, bIsSparse);
    }
    return retval;
}
//=============================================================================
