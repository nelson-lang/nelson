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
#include "DisplaySparseHelpers.hpp"
#include "DisplayVariableHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
sparseIndexText(const ArrayOf& A, indexType row, indexType column)
{
    // width of "(row" and of "column)", by size of the largest dimension
    indexType maxDimension = std::max(A.rows(), A.columns());
    size_t width = 20;
    if (maxDimension < 10000) {
        width = 5;
    } else if (maxDimension < 100000) {
        width = 11;
    }
    return completeWithBlanksAtBeginning(L"(" + std::to_wstring(row), width) + L","
        + completeWithBlanksAtTheEnd(std::to_wstring(column) + L")", width);
}
//=============================================================================
bool
DisplaySparseBeginning(
    Interface* io, const ArrayOf& A, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && !asDisp) {
        io->outputMessage(L"\n");
    }
    if (A.nonzeros() == 0) {
        io->outputMessage(L"   " + _W("All zero") + L"\n");
        DisplaySparseEnd(io, currentLineSpacing, asDisp);
        return false;
    }
    return true;
}
//=============================================================================
void
DisplaySparseEnd(Interface* io, LineSpacingDisplay currentLineSpacing, bool asDisp)
{
    if (currentLineSpacing == NLS_LINE_SPACING_LOOSE && asDisp) {
        io->outputMessage(L"\n");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
