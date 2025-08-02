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
{
    this->_withWatcher = withWatcher;
    this->_isOverload = isOverload;
    _ismex = ismex;
    _fullfilename = buildFullFilename(directory, objectName, name, ismex, isPrivate);
    _name = name;
}
//=============================================================================
std::wstring
FileFunction::buildFullFilename(const std::wstring& directory, const std::wstring& objectName,
    const std::wstring& name, bool ismex, bool isPrivate)
{
    std::wstring _fullfilename;
    std::wstring extension = ismex ? L"." + getMexExtension() : L".m";

    if (isPrivate) {
        if (objectName.empty()) {
            _fullfilename = directory + L"/" + name + extension;
        } else {
            _fullfilename = directory + L"/" + L"@" + objectName + L"/" + extension;
        }
    } else {
        if (objectName.empty() || objectName != name) {
            _fullfilename = directory + L"/" + name + extension;
        } else {
            _fullfilename = directory + L"/" + L"@" + objectName + L"/" + name + extension;
        }
    }
    return _fullfilename;
}
//=============================================================================
FileFunction::~FileFunction()
{
    _fullfilename.clear();
    _name.clear();
    _ismex = false;
}
//=============================================================================
std::wstring
FileFunction::getFilename()
{
    return _fullfilename;
}
//=============================================================================
std::wstring
FileFunction::getName()
{
    return _name;
}
//=============================================================================
bool
FileFunction::isMex()
{
    return _ismex;
}
//=============================================================================
bool
FileFunction::getWithWatcher()
{
    return _withWatcher;
}
//=============================================================================
bool
FileFunction::isOverload()
{
    return _isOverload;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
