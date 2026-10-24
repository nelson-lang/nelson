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
#include "appdataBuiltins.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
// Application data lives in a session-wide side table keyed by the object's
// handle, so no storage is added to the graphics object itself. Entries persist
// until explicitly removed; they are not tied to object deletion.
static std::map<nelson_handle, std::map<std::wstring, ArrayOf>>&
appDataStore()
{
    static std::map<nelson_handle, std::map<std::wstring, ArrayOf>> store;
    return store;
}
//=============================================================================
static nelson_handle
graphicsHandleArgument(const ArrayOf& arg)
{
    if (arg.dataClass() != NLS_GO_HANDLE || !arg.isScalar()) {
        Error(L"Nelson:appdata:invalidHandle", _W("A scalar graphics object handle is expected."));
    }
    return ((nelson_handle*)arg.dataPointer())[0];
}
//=============================================================================
static std::wstring
nameArgument(const ArrayOf& arg)
{
    if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
        Error(L"Nelson:appdata:invalidName", _W("NAME must be a string."));
    }
    return arg.asWideString();
}
//=============================================================================
ArrayOfVector
setappdataBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 0);
    nelson_handle handle = graphicsHandleArgument(argIn[0]);
    std::wstring name = nameArgument(argIn[1]);
    appDataStore()[handle][name] = argIn[2];
    return {};
}
//=============================================================================
ArrayOfVector
getappdataBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    nelson_handle handle = graphicsHandleArgument(argIn[0]);
    auto& store = appDataStore();
    auto objectIt = store.find(handle);
    ArrayOfVector retval;
    if (argIn.size() == 2) {
        std::wstring name = nameArgument(argIn[1]);
        if (objectIt != store.end()) {
            auto valueIt = objectIt->second.find(name);
            if (valueIt != objectIt->second.end()) {
                retval << valueIt->second;
                return retval;
            }
        }
        retval << ArrayOf::empty(Dimensions(0, 0));
        return retval;
    }
    // getappdata(h): every stored value as a scalar structure.
    wstringVector fieldnames;
    ArrayOfVector values;
    if (objectIt != store.end()) {
        for (const auto& entry : objectIt->second) {
            fieldnames.push_back(entry.first);
            values.push_back(entry.second);
        }
    }
    retval << ArrayOf::structScalar(fieldnames, values);
    return retval;
}
//=============================================================================
ArrayOfVector
isappdataBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    nelson_handle handle = graphicsHandleArgument(argIn[0]);
    std::wstring name = nameArgument(argIn[1]);
    auto& store = appDataStore();
    auto objectIt = store.find(handle);
    bool present = objectIt != store.end() && objectIt->second.find(name) != objectIt->second.end();
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(present);
    return retval;
}
//=============================================================================
ArrayOfVector
rmappdataBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);
    nelson_handle handle = graphicsHandleArgument(argIn[0]);
    std::wstring name = nameArgument(argIn[1]);
    auto& store = appDataStore();
    auto objectIt = store.find(handle);
    if (objectIt == store.end() || objectIt->second.find(name) == objectIt->second.end()) {
        Error(L"Nelson:appdata:notFound", _W("The application-defined data does not exist."));
    }
    objectIt->second.erase(name);
    if (objectIt->second.empty()) {
        store.erase(objectIt);
    }
    return {};
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
