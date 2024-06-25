//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    bool _followLocation;

public:
    WebOptions(ArrayOf& webOptionsArrayOf);
    ~WebOptions();
    std::wstring
    getCharacterEncoding() const;
    std::wstring
    getUserAgent() const;
    double
    getTimeout() const;
    std::wstring
    getUsername() const;
    std::wstring
    getPassword() const;
    std::wstring
    getKeyName() const;
    ArrayOf
    getKeyValue() const;
    wstringVector
    getHeaderFields() const;
    std::wstring
    getContentType() const;
    function_handle
    getContentReader() const;
    std::wstring
    getMediaType() const;
    std::wstring
    getRequestMethod() const;
    std::wstring
    getArrayformat() const;
    std::wstring
    getCertificateFilename() const;
    bool
    getFollowLocation() const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
