//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include <fstream>
#include <iosfwd>
#include "FileFunction.hpp"
#include "characters_encoding.hpp"
#include "MxGetExtension.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FileFunction::FileFunction(
    const std::wstring& directory, const std::wstring& name, bool ismex, bool withWatcher)
{
    this->_withWatcher = withWatcher;
    bool withFinalDirectorySeparator = boost::algorithm::ends_with(directory, L"/")
        || boost::algorithm::ends_with(directory, L"\\");
    _ismex = ismex;
    _fullfilename = directory;
    if (!withFinalDirectorySeparator) {
        _fullfilename = _fullfilename + L"/";
    }
    if (ismex) {
        _fullfilename = _fullfilename + name + L"." + getMexExtension();
    } else {
        _fullfilename = _fullfilename + name + L".m";
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
} // namespace Nelson
//=============================================================================
