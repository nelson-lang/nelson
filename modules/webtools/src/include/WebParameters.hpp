//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsWebtools_exports.h"
#include "ArrayOf.hpp"
#include "WebOptions.hpp"
//=============================================================================
// Request encoding shared by the HTTP backends (libcurl natively, the browser
// network stack in the WebAssembly build): query/form parameters, header lines.
//=============================================================================
namespace Nelson {
//=============================================================================
// Percent-encodes everything but the RFC 3986 unreserved characters.
NLSWEBTOOLS_IMPEXP std::string
urlEscape(const std::string& text);
//=============================================================================
// name1=value1&name2=value2 from webread/webwrite parameter pairs, honoring
// weboptions.ArrayFormat for non-scalar values.
NLSWEBTOOLS_IMPEXP std::string
formEncode(const stringVector& names, const ArrayOfVector& values, WebOptions& options);
//=============================================================================
// The URL with the parameters appended as a query string.
NLSWEBTOOLS_IMPEXP std::string
encodeUrl(const std::wstring& url, const stringVector& names, const ArrayOfVector& values,
    WebOptions& options);
//=============================================================================
// "Name: value" request header lines derived from weboptions (character set,
// key name, media type, HeaderFields). Throws on an odd HeaderFields count.
NLSWEBTOOLS_IMPEXP stringVector
buildHeaderLines(const WebOptions& options);
//=============================================================================
} // namespace Nelson
//=============================================================================
