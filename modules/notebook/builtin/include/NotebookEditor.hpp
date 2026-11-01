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
#include "Evaluator.hpp"
#include <string>
//=============================================================================
namespace Nelson::NotebookGateway {
//=============================================================================
// Open a document in the Notebook editor; an empty path is a new one. False without a desktop.
bool
openNotebookEditor(Evaluator* eval, const std::wstring& filename);
//=============================================================================
// Close the Notebook window of the Qt desktop, if it was opened.
void
closeNotebookWindow();
//=============================================================================
} // namespace Nelson::NotebookGateway
//=============================================================================
