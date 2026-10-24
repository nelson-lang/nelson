//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include "IsValidGraphicsProperty.hpp"
#include "GOAxis.hpp"
#include "GraphicsObject.hpp"
#include "GraphicsObjectFactory.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Property names for a graphics type, built once from a throwaway object and
// cached. Graphics runs on the interpreter thread, so a plain static map is
// enough; the vector keeps the registration order the prefix resolver expects.
static const wstringVector&
cachedPropertyNames(const std::wstring& GOTypename)
{
    static std::map<std::wstring, wstringVector> cache;
    auto found = cache.find(GOTypename);
    if (found != cache.end()) {
        return found->second;
    }
    wstringVector names;
    GraphicsObject* fp = newGraphicsObjectForType(GOTypename, nullptr, true);
    if (fp) {
        names = fp->fieldnames();
        delete fp;
    }
    return cache.emplace(GOTypename, std::move(names)).first->second;
}
//=============================================================================
bool
IsValidGraphicsProperty(const std::wstring& GOTypename, const std::wstring& GOPropertyName)
{
    // Exact-name probe used by the .m plotting wrappers to tell a property name from positional
    // data (a colour spec, a bin count, ...). It must stay an exact match: resolving abbreviations
    // here would turn a positional 'y' (colour) into the 'YData' prefix. Partial-name resolution
    // is offered through CanonicalGraphicsProperty, which the wrappers apply only where a property
    // is actually set.
    const wstringVector& names = cachedPropertyNames(GOTypename);
    if (names.empty()) {
        Error(L"Nelson:graphics:invalidTypeName", L"Invalid Graphics object type name:%s",
            GOTypename);
        return false;
    }
    if (StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_AXES_STR)
        && GOAxis::isPolarAxesOnlyProperty(GOPropertyName)) {
        return false;
    }
    if (StringHelpers::iequals(GOTypename, GO_PROPERTY_VALUE_POLAR_AXES_STR)
        && GOAxis::isCartesianAxesOnlyProperty(GOPropertyName)) {
        return false;
    }
    for (const std::wstring& name : names) {
        if (StringHelpers::iequals(name, GOPropertyName)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
std::wstring
CanonicalGraphicsProperty(const std::wstring& GOTypename, const std::wstring& GOPropertyName)
{
    const wstringVector& names = cachedPropertyNames(GOTypename);
    if (names.empty()) {
        return std::wstring();
    }
    std::wstring canonical = GraphicsObject::resolvePartialPropertyName(GOPropertyName, names);
    for (const std::wstring& name : names) {
        if (StringHelpers::iequals(name, canonical)) {
            return name;
        }
    }
    return std::wstring();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
