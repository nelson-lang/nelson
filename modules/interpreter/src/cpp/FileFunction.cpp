//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include <fstream>
#include <iosfwd>
#include "FileFunction.hpp"
#include "characters_encoding.hpp"
#include "MxGetExtension.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FileFunction::FileFunction(const std::wstring& directory, const std::wstring& objectName,
    const std::wstring& name, bool ismex, bool withWatcher, bool isOverload, bool isPrivate)
    : _withWatcher(withWatcher)
    , _isOverload(isOverload)
    , _ismex(ismex)
    , _name(name)
    , _isPrivate(isPrivate)
{
    _fullfilename = buildFullFilename(directory, objectName, name, ismex, isPrivate);
}
//=============================================================================
std::wstring
FileFunction::buildFullFilename(const std::wstring& directory, const std::wstring& objectName,
    const std::wstring& name, bool ismex, bool isPrivate)
{
    const std::wstring extension = ismex ? L"." + getMexExtension() : L".m";
    std::wstring path = directory + L"/";

    if (isPrivate) {
        // Private function - stored under directory/@objectName/
        if (!objectName.empty()) {
            path += L"@" + objectName + L"/";
        }
        path += name + extension;
    } else {
        // Public function
        if (!objectName.empty() && objectName == name) {
            path += L"@" + objectName + L"/";
        }
        path += name + extension;
    }
    return path;
}
//=============================================================================
FileFunction::~FileFunction() { }
//=============================================================================
const std::wstring&
FileFunction::getFilename() const
{
    return _fullfilename;
}
//=============================================================================
const std::wstring&
FileFunction::getName() const
{
    return _name;
}
//=============================================================================
bool
FileFunction::isMex() const
{
    return _ismex;
}
//=============================================================================
bool
FileFunction::getWithWatcher() const
{
    return _withWatcher;
}
//=============================================================================
bool
FileFunction::isOverload() const
{
    return _isOverload;
}
//=============================================================================
bool
FileFunction::isPrivate() const
{
    return _isPrivate;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
