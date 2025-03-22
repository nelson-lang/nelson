//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Interface.hpp"
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "nlsGraphics_io_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGRAPHICS_IO_IMPEXP ArrayOf
imageFormatDisplay(Evaluator* eval);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP ArrayOf
imageSupport(Evaluator* eval);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP ArrayOf
imageSupport(Evaluator* eval, const std::wstring& format);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP bool
isImageFormatReadable(const std::wstring& format);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP bool
isImageFormatWritable(const std::wstring& format);
//=============================================================================
}
//=============================================================================
// namespace Nelson
//=============================================================================
