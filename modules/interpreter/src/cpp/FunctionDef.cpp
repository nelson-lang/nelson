//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "FunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FunctionDef::FunctionDef(bool isOverload) { this->_isOverload = isOverload; };
//=============================================================================
FunctionDef::~FunctionDef() = default;
//=============================================================================
void
FunctionDef::setFilename(const std::wstring& filename)
{
    this->filename = filename;
    FileSystemWrapper::Path path(filename);
    this->pathname = path.parent_path().generic_wstring();
}
//=============================================================================
}
//=============================================================================
