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
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/* internal converters, numbers previous passed OK with lexer */
//=============================================================================
NLSINTERPRETER_IMPEXP double
textToDouble(const std::string& str);
NLSINTERPRETER_IMPEXP single
textToSingle(const std::string& str);
//=============================================================================
NLSINTERPRETER_IMPEXP uint8
textToUint8(const std::string& str);
NLSINTERPRETER_IMPEXP int8
textToInt8(const std::string& str);
//=============================================================================
NLSINTERPRETER_IMPEXP uint16
textToUint16(const std::string& str);
NLSINTERPRETER_IMPEXP int16
textToInt16(const std::string& str);
//=============================================================================
NLSINTERPRETER_IMPEXP uint32
textToUint32(const std::string& str);
NLSINTERPRETER_IMPEXP int32
textToInt32(const std::string& str);
//=============================================================================
NLSINTERPRETER_IMPEXP int64
textToInt64(const std::string& str);
NLSINTERPRETER_IMPEXP uint64
textToUint64(const std::string& str);
//=============================================================================
}; // namespace Nelson
//=============================================================================
