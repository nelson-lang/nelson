//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebOptions.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
WebOptions::WebOptions(ArrayOf& webOptionsArrayOf)
{
    if (webOptionsArrayOf.getClassType() != "weboptions") {
        Error(_W("weboptions object expected."));
    }
    ArrayOf res;
    res = webOptionsArrayOf.getField("CharacterEncoding");
    _characterEncoding = res.getContentAsWideString();
    if (_characterEncoding == L"auto") {
        _characterEncoding = L"UTF-8";
    }

    res = webOptionsArrayOf.getField("UserAgent");
    _userAgent = res.getContentAsWideString();

    res = webOptionsArrayOf.getField("Timeout");
    _timeout = res.getContentAsDoubleScalar();

    res = webOptionsArrayOf.getField("Username");
    _username = res.getContentAsWideString();

    res = webOptionsArrayOf.getField("Password");
    _password = res.getContentAsWideString();

    res = webOptionsArrayOf.getField("KeyName");
    _keyName = res.getContentAsWideString();

    _keyValue = webOptionsArrayOf.getField("KeyValue");

    res = webOptionsArrayOf.getField("HeaderFields");
    if (!res.isEmpty()) {
        _headerFields = res.getContentAsWideStringVector(false);
    } else {
        _headerFields.clear();
    }

    res = webOptionsArrayOf.getField("ContentType");
    _contentType = res.getContentAsWideString();

    res = webOptionsArrayOf.getField("ContentReader");
    if (!res.isEmpty()) {
        _contentReader = res.getContentAsFunctionHandle();
    }

    res = webOptionsArrayOf.getField("MediaType");
    _mediaType = res.getContentAsWideString();

    res = webOptionsArrayOf.getField("RequestMethod");
    _requestMethod = res.getContentAsWideString();

    res = webOptionsArrayOf.getField("ArrayFormat");
    _arrayformat = res.getContentAsWideString();

    res = webOptionsArrayOf.getField("CertificateFilename");
    if (!res.isEmpty()) {
        _certificateFilename = res.getContentAsWideString();
    } else {
        _certificateFilename.clear();
    }

    res = webOptionsArrayOf.getField("FollowLocation");
    if (!res.isEmpty()) {
        _followLocation = res.getContentAsLogicalScalar();
    } else {
        _followLocation = false;
    }
}
//=============================================================================
WebOptions::~WebOptions()
{
    _characterEncoding.clear();
    _userAgent.clear();
    _timeout = 5;
    _username.clear();
    _password.clear();
    _keyName.clear();
    _keyValue = ArrayOf::emptyConstructor();
    _headerFields.clear();
    _contentType.clear();
    _contentReader.anonymousHandle = nullptr;
    _mediaType.clear();
    _requestMethod.clear();
    _arrayformat.clear();
    _certificateFilename.clear();
    _followLocation = false;
}
//=============================================================================
std::wstring
WebOptions::getCharacterEncoding() const
{
    return _characterEncoding;
}
//=============================================================================
std::wstring
WebOptions::getUserAgent() const
{
    return _userAgent;
}
//=============================================================================
double
WebOptions::getTimeout() const
{
    return _timeout;
}
//=============================================================================
std::wstring
WebOptions::getUsername() const
{
    return _username;
}
//=============================================================================
std::wstring
WebOptions::getPassword() const
{
    return _password;
}
//=============================================================================
std::wstring
WebOptions::getKeyName() const
{
    return _keyName;
}
//=============================================================================
ArrayOf
WebOptions::getKeyValue() const
{
    return _keyValue;
}
//=============================================================================
wstringVector
WebOptions::getHeaderFields() const
{
    return _headerFields;
}
//=============================================================================
std::wstring
WebOptions::getContentType() const
{
    return _contentType;
}
//=============================================================================
function_handle
WebOptions::getContentReader() const
{
    return _contentReader;
}
//=============================================================================
std::wstring
WebOptions::getMediaType() const
{
    return _mediaType;
}
//=============================================================================
std::wstring
WebOptions::getRequestMethod() const
{
    return _requestMethod;
}
//=============================================================================
std::wstring
WebOptions::getArrayformat() const
{
    return _arrayformat;
}
//=============================================================================
std::wstring
WebOptions::getCertificateFilename() const
{
    return _certificateFilename;
}
//=============================================================================
bool
WebOptions::getFollowLocation() const
{
    return _followLocation;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
