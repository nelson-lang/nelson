//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "ClassToString.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Nelson:invalidConversion already carries another template in the
// identifier-keyed catalog, so the message is resolved text-keyed and raised
// with RaiseError.
//=============================================================================
// A function handle, a struct or a sparse array has no string form, even when
// empty: the first one in argument order names the error.
inline void
checkStringConcatenationOperands(const ArrayOfVector& args)
{
    for (const ArrayOf& arg : args) {
        if (arg.isFunctionHandle() || arg.isStruct() || arg.isSparse()) {
            std::string name = ClassToString(arg.dataClass());
            if (arg.isSparse()) {
                name = "sparse " + name;
            }
            RaiseError(formatI18nMessage(_("Conversion to string from %s is not possible."), name),
                "Nelson:invalidConversion");
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
