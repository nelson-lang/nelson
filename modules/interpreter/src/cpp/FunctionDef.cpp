//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include "FunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FunctionDef::FunctionDef() = default;
//=============================================================================
FunctionDef::~FunctionDef() = default;
//=============================================================================
void
FunctionDef::setFilename(const std::wstring& filename)
{
    this->filename = filename;
    boost::filesystem::path path(filename);
    this->pathname = path.parent_path().generic_wstring();
    try {
        this->timestamp = boost::filesystem::last_write_time(filename);
    } catch (const boost::filesystem::filesystem_error&) {
        this->timestamp = 0;
    }
}
//=============================================================================
}
//=============================================================================
