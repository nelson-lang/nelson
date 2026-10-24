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
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class ArrayOf;
class Evaluator;
//=============================================================================
NLSINTERPRETER_IMPEXP bool
classdefPrepareSaveObject(Evaluator* eval, const ArrayOf& objectValue, ArrayOf& savedValue);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
classdefPreparePersistentObject(const ArrayOf& objectValue, ArrayOf& savedValue);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
classdefToStruct(const ArrayOf& objectValue, ArrayOf& structValue);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
classdefRestoreLoadObject(
    Evaluator* eval, const std::string& className, const ArrayOf& savedValue, ArrayOf& objectValue);
//=============================================================================
NLSINTERPRETER_IMPEXP bool
classdefRestoreDefaultValues(
    Evaluator* eval, const std::string& className, const ArrayOf& savedValue, ArrayOf& objectValue);
//=============================================================================
} // namespace Nelson
//=============================================================================
