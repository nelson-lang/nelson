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
#include "FileSystemHelpers.hpp"
#include "FunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FunctionDef::FunctionDef() = default;
//=============================================================================
FunctionDef::~FunctionDef() = default;
//=============================================================================
template <typename TP>
std::time_t
to_time_t(TP tp)
{
    using namespace std::chrono;
    auto sctp
        = time_point_cast<system_clock::duration>(tp - TP::clock::now() + system_clock::now());
    return system_clock::to_time_t(sctp);
}
//=============================================================================
void
FunctionDef::setFilename(const std::wstring& filename)
{
    this->filename = filename;
    std::filesystem::path path = createFileSystemPath(filename);
    this->pathname = convertFileSytemPathToGenericWString(path.parent_path());
    try {
        this->timestamp = to_time_t(std::filesystem::last_write_time(filename));
    } catch (const std::filesystem::filesystem_error&) {
        this->timestamp = 0;
    }
}
//=============================================================================
}
//=============================================================================
