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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsWebtools_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSWEBTOOLS_IMPEXP WebOptions
{
private:
    std::wstring _characterEncoding;
    std::wstring _userAgent;
    double _timeout;
    std::wstring _username;
    std::wstring _password;
    std::wstring _keyName;
    ArrayOf _keyValue;
    wstringVector _headerFields;
    std::wstring _contentType;
    function_handle _contentReader;
    std::wstring _mediaType;
    std::wstring _requestMethod;
    std::wstring _arrayformat;
    std::wstring _certificateFilename;

public:
    WebOptions(ArrayOf& webOptionsArrayOf);
    ~WebOptions();
    std::wstring
    getCharacterEncoding();
    std::wstring
    getUserAgent();
    double
    getTimeout();
    std::wstring
    getUsername();
    std::wstring
    getPassword();
    std::wstring
    getKeyName();
    ArrayOf
    getKeyValue();
    wstringVector
    getHeaderFields();
    std::wstring
    getContentType();
    function_handle
    getContentReader();
    std::wstring
    getMediaType();
    std::wstring
    getRequestMethod();
    std::wstring
    getArrayformat();
    std::wstring
    getCertificateFilename();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
