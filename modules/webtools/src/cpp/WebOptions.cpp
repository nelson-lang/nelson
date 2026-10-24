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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    ArrayOf
    getWebOptionField(const ArrayOf& webOptionsArrayOf, const std::string& fieldName)
    {
        if (webOptionsArrayOf.isField(fieldName)) {
            return webOptionsArrayOf.field(fieldName);
        }
        std::string storageFieldName = fieldName + "Value";
        if (webOptionsArrayOf.isField(storageFieldName)) {
            return webOptionsArrayOf.field(storageFieldName);
        }
        Error("Nelson:nonExistentField", "Reference to non-existent field %s", fieldName);
        return ArrayOf::empty();
    }
    //=============================================================================
} // namespace
//=============================================================================
WebOptions::WebOptions(ArrayOf& webOptionsArrayOf)
{
    if (webOptionsArrayOf.classType() != "weboptions") {
        Error(L"Nelson:weboptions:objectExpected", L"weboptions object expected.");
    }
    ArrayOf res;
    res = getWebOptionField(webOptionsArrayOf, "CharacterEncoding");
    _characterEncoding = res.asWideString();
    if (_characterEncoding == L"auto") {
        _characterEncoding = L"UTF-8";
    }

    res = getWebOptionField(webOptionsArrayOf, "UserAgent");
    _userAgent = res.asWideString();

    res = getWebOptionField(webOptionsArrayOf, "Timeout");
    _timeout = res.asDoubleScalar();

    res = getWebOptionField(webOptionsArrayOf, "Username");
    _username = res.asWideString();

    res = getWebOptionField(webOptionsArrayOf, "Password");
    _password = res.asWideString();

    res = getWebOptionField(webOptionsArrayOf, "KeyName");
    _keyName = res.asWideString();

    _keyValue = getWebOptionField(webOptionsArrayOf, "KeyValue");

    res = getWebOptionField(webOptionsArrayOf, "HeaderFields");
    if (!res.isEmpty()) {
        _headerFields = res.asWideStringVector(false);
    } else {
        _headerFields.clear();
    }

    res = getWebOptionField(webOptionsArrayOf, "ContentType");
    _contentType = res.asWideString();

    res = getWebOptionField(webOptionsArrayOf, "ContentReader");
    if (!res.isEmpty()) {
        _contentReader = res.asFunctionHandle();
    }

    res = getWebOptionField(webOptionsArrayOf, "MediaType");
    _mediaType = res.asWideString();

    res = getWebOptionField(webOptionsArrayOf, "RequestMethod");
    _requestMethod = res.asWideString();

    res = getWebOptionField(webOptionsArrayOf, "ArrayFormat");
    _arrayformat = res.asWideString();

    res = getWebOptionField(webOptionsArrayOf, "CertificateFilename");
    if (!res.isEmpty()) {
        _certificateFilename = res.asWideString();
    } else {
        _certificateFilename.clear();
    }

    res = getWebOptionField(webOptionsArrayOf, "FollowLocation");
    if (!res.isEmpty()) {
        _followLocation = res.asLogicalScalar();
    } else {
        _followLocation = true;
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
    _keyValue = ArrayOf::empty();
    _headerFields.clear();
    _contentType.clear();
    _contentReader.anonymousHandle = nullptr;
    _mediaType.clear();
    _requestMethod.clear();
    _arrayformat.clear();
    _certificateFilename.clear();
    _followLocation = true;
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
