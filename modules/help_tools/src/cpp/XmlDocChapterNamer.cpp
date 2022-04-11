//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#include "XmlDocChapterNamer.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
XmlDocChapterNamer(const std::wstring& dstDirectory)
{
    boost::uuids::uuid uuid = boost::uuids::random_generator()();
    std::wstring guid = boost::uuids::to_wstring(uuid);
    return dstDirectory + L"chapter-" + guid + L".html";
}
//=============================================================================
}
//=============================================================================
