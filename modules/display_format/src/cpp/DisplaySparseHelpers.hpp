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
#include "Interface.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// "   (r,c)   " written before a nonzero element: the row index right-aligned
// and the column index left-aligned around a comma placed by the size of A.
std::wstring
sparseIndexText(const ArrayOf& A, indexType row, indexType column);
//=============================================================================
// Blank line after the header, "All zero" when A has no nonzero element.
// Returns true when the nonzero elements remain to be displayed.
bool
DisplaySparseBeginning(
    Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
void
DisplaySparseEnd(Interface* io, LineSpacingDisplay currentLineSpacing, bool asDisp);
//=============================================================================
} // namespace Nelson
//=============================================================================
