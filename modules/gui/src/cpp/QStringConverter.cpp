//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
//=============================================================================
std::wstring
Nelson::QStringTowstring(QString qstr)
{
    return Nelson::utf8_to_wstring(qstr.toUtf8().constData());
}
//=============================================================================
QString
Nelson::wstringToQString(const std::wstring& wstr)
{
    return QString::fromUtf8(Nelson::wstring_to_utf8(wstr).c_str());
}
//=============================================================================
