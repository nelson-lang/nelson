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
#include <vector>
#include "nlsParallel_exports.h"
#include "ArrayOf.hpp"
#include "FutureObject.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSPARALLEL_IMPEXP
ArrayOf
FuturesToArrayOf(const std::vector<FutureObject*>& futures);
//=============================================================================
NLSPARALLEL_IMPEXP
std::vector<FutureObject*>
ArrayOfToFutures(const ArrayOf& _param);
//=============================================================================
NLSPARALLEL_IMPEXP
ArrayOfVector
vertCatArrayOfVector(const ArrayOfVector& args1, const ArrayOfVector& args2, Exception& e);
}
//=============================================================================
