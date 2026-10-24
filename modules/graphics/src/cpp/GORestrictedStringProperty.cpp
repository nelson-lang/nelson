//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cwctype>
#include "GORestrictedStringProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
wideStringEqualsIgnoreCase(const std::wstring& lhs, const std::wstring& rhs)
{
    if (lhs.size() != rhs.size()) {
        return false;
    }
    for (size_t k = 0; k < lhs.size(); ++k) {
        if (std::towlower(lhs[k]) != std::towlower(rhs[k])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
wideStringIsPrefixIgnoreCase(const std::wstring& prefix, const std::wstring& full)
{
    if (prefix.empty() || prefix.size() > full.size()) {
        return false;
    }
    for (size_t k = 0; k < prefix.size(); ++k) {
        if (std::towlower(prefix[k]) != std::towlower(full[k])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
GORestrictedStringProperty::GORestrictedStringProperty(const wchar_t** dict)
{
    while (*dict) {
        m_dictionary.emplace_back(*dict);
        dict++;
    }
}
//=============================================================================
void
GORestrictedStringProperty::set(ArrayOf arg)
{
    if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
        Error(L"Nelson:graphics:expectedStringProperty", L"Expecting a string for property.");
    }
    std::wstring tst(arg.asWideString());
    auto match = std::find_if(m_dictionary.begin(), m_dictionary.end(),
        [&tst](const std::wstring& value) { return wideStringEqualsIgnoreCase(value, tst); });
    if (match == m_dictionary.end()) {
        // Accepts the shortest unambiguous leading substring of an enumerated value
        // (e.g. EdgeColor 'n' -> 'none', Projection 'p' -> 'perspective', FaceLighting 'g' ->
        // 'gouraud').
        const std::wstring* unique = nullptr;
        bool ambiguous = false;
        for (const auto& value : m_dictionary) {
            if (!wideStringIsPrefixIgnoreCase(tst, value)) {
                continue;
            }
            if (unique == nullptr) {
                unique = &value;
            } else {
                ambiguous = true;
                break;
            }
        }
        if (unique != nullptr && !ambiguous) {
            match = m_dictionary.begin() + (unique - &m_dictionary[0]);
        }
    }
    if (match == m_dictionary.end()) {
        Error(L"Nelson:graphics:illegalSelection", L"Illegal selection for property.");
    }
    if (data() != *match) {
        GOStringProperty::set(ArrayOf::characterArray(*match));
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
