//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
FileFunction::FileFunction(const std::wstring& directory, const std::wstring& name, bool ismex,
    bool withWatcher, bool isOverload, bool isPrivate)
{
    this->_withWatcher = withWatcher;
    this->_isOverload = isOverload;
    _ismex = ismex;
    _fullfilename = directory;
    if (isPrivate) {
        if (ismex) {
            _fullfilename = name + L"." + getMexExtension();
        } else {
            _fullfilename = name + L".m";
        }
    } else {
        if (ismex) {
            _fullfilename = _fullfilename + L"/" + name + L"." + getMexExtension();
        } else {
            _fullfilename = _fullfilename + L"/" + name + L".m";
        }
    }
    _name = name;
    std::ifstream inFile;
#ifdef _MSC_VER
    inFile.open(_fullfilename);
#else
    inFile.open(wstring_to_utf8(_fullfilename));
#endif
    if (inFile.is_open()) {
        inFile.close();
    }
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
