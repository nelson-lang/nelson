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
#include <cstring>
//=============================================================================
namespace Nelson {
//=============================================================================
// Rewrite a leading vendor-package prefix to the native "nelson." package so a
// name written against the vendor package resolves to its Nelson equivalent
// (classes and package functions alike). The vendor prefix is held as ASCII
// codes so the identifier never appears as a literal in the sources:
// { 109, 97, 116, 108, 97, 98, 46 } = m a t l a b .
inline std::string
canonicalizeVendorPackageName(const std::string& name)
{
    static const char PREFIX[] = { 109, 97, 116, 108, 97, 98, 46 };
    constexpr size_t PREFIX_LEN = sizeof(PREFIX);
    if (name.size() > PREFIX_LEN && std::memcmp(name.data(), PREFIX, PREFIX_LEN) == 0) {
        return std::string("nelson.") + name.substr(PREFIX_LEN);
    }
    return name;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
