//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PreferencesStore.hpp"
#include <cstdlib>
#include <mutex>
#ifndef H5_BUILT_AS_DYNAMIC_LIB
#define H5_BUILT_AS_DYNAMIC_LIB
#endif
#include <hdf5.h>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FileSystemWrapper.hpp"
#include "IsValidVariableName.hpp"
#include "NelsonConfiguration.hpp"
#include "NelsonException.hpp"
#include "characters_encoding.hpp"
#include "h5LoadVariable.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "h5SaveVariable.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::Preferences {
//=============================================================================
namespace {
    constexpr auto VALUES_FIELD = "values";
    constexpr auto UI_REGISTRY_FIELD = "uigetpref";
    constexpr auto EMPTY_SENTINEL_FIELD = "__nelson_empty__";
    std::mutex storeMutex;
    //=============================================================================
    ArrayOf
    emptyScalarStruct()
    {
        return ArrayOf::structScalar(stringVector(), ArrayOfVector());
    }
    //=============================================================================
    ArrayOf
    storageScalarStruct(const ArrayOf& value)
    {
        if (value.isStruct() && value.isScalar() && value.fieldNames().empty()) {
            stringVector fields { EMPTY_SENTINEL_FIELD };
            ArrayOfVector values;
            values << ArrayOf::logicalScalar(true);
            return ArrayOf::structScalar(fields, values);
        }
        return value;
    }
    //=============================================================================
    ArrayOf
    publicScalarStruct(const ArrayOf& value)
    {
        if (!value.isStruct() || !value.isScalar()) {
            Error(L"Nelson:preferences:invalidStore", L"Invalid preferences store.");
        }
        stringVector names = value.fieldNames();
        if (names.size() == 1 && names[0] == EMPTY_SENTINEL_FIELD) {
            return emptyScalarStruct();
        }
        return value;
    }
    //=============================================================================
    std::string
    toFieldName(const std::wstring& name)
    {
        return wstring_to_utf8(name);
    }
    //=============================================================================
    std::string
    getEnvironmentVariable(const char* name)
    {
#ifdef _MSC_VER
        char* value = nullptr;
        size_t length = 0;
        if (_dupenv_s(&value, &length, name) != 0 || value == nullptr) {
            return {};
        }
        std::string result(value, length > 0 ? length - 1 : 0);
        free(value);
        return result;
#else
        const char* value = std::getenv(name);
        return value == nullptr ? std::string() : std::string(value);
#endif
    }
    //=============================================================================
    std::wstring
    preferencesDirectoryPath()
    {
        std::string overrideDirectory = getEnvironmentVariable("NELSON_PREFERENCES_PATH");
        if (!overrideDirectory.empty()) {
            return utf8_to_wstring(overrideDirectory);
        }
        return NelsonConfiguration::getInstance()->getNelsonPreferencesDirectory();
    }
    //=============================================================================
    void
    validateName(const std::wstring& name, const std::wstring& argumentName)
    {
        if (!IsValidVariableName(name)) {
            Error(L"Nelson:validators:mustBeValidVariableName",
                L"%s must be a valid variable name.", argumentName);
        }
    }
    //=============================================================================
    ArrayOf
    makeRoot(ArrayOf values, ArrayOf registry)
    {
        stringVector fields { VALUES_FIELD, UI_REGISTRY_FIELD };
        ArrayOfVector fieldValues;
        fieldValues << storageScalarStruct(values);
        fieldValues << storageScalarStruct(registry);
        return ArrayOf::structScalar(fields, fieldValues);
    }
    //=============================================================================
    struct Store
    {
        ArrayOf values;
        ArrayOf registry;
    };
    //=============================================================================
    Store
    emptyStore()
    {
        return { emptyScalarStruct(), emptyScalarStruct() };
    }
    //=============================================================================
    Store
    loadStoreUnlocked()
    {
        FileSystemWrapper::Path path(preferencesFilePath());
        bool permissionDenied = false;
        if (!FileSystemWrapper::Path::is_regular_file(path, permissionDenied)) {
            if (permissionDenied) {
                Error(L"Nelson:preferences:permissionDenied", L"Permission denied.");
            }
            return emptyStore();
        }
        hid_t fid = H5Fopen(wstring_to_utf8(path.wstring()).c_str(), H5F_ACC_RDONLY, H5P_DEFAULT);
        if (fid == H5I_INVALID_HID) {
            return emptyStore();
        }
        ArrayOf root;
        try {
            if (!h5LoadVariable(nullptr, fid, "/", "preferences", root)) {
                H5Fclose(fid);
                return emptyStore();
            }
        } catch (const NelsonException&) {
            H5Fclose(fid);
            return emptyStore();
        }
        H5Fclose(fid);
        if (!root.isStruct() || !root.isScalar()) {
            Error(L"Nelson:preferences:invalidStore", L"Invalid preferences store.");
        }
        Store store = emptyStore();
        if (root.isField(VALUES_FIELD)) {
            store.values = publicScalarStruct(root.field(VALUES_FIELD));
        }
        if (root.isField(UI_REGISTRY_FIELD)) {
            store.registry = publicScalarStruct(root.field(UI_REGISTRY_FIELD));
        }
        if (!store.values.isStruct() || !store.values.isScalar() || !store.registry.isStruct()
            || !store.registry.isScalar()) {
            Error(L"Nelson:preferences:invalidStore", L"Invalid preferences store.");
        }
        return store;
    }
    //=============================================================================
    void
    saveStoreUnlocked(const Store& store)
    {
        ArrayOf root = makeRoot(store.values, store.registry);
        hid_t fid = createNh5FileWithHeader(preferencesFilePath());
        if (fid == H5I_INVALID_HID) {
            Error(L"Nelson:preferences:cannotOpenStore", L"Cannot open preferences store.");
        }
        updateNelsonH5Header(fid);
        if (!h5SaveVariable(nullptr, fid, "/", "preferences", root, true)) {
            H5Fclose(fid);
            Error(L"Nelson:preferences:cannotWriteStore", L"Cannot write preferences store.");
        }
        H5Fclose(fid);
    }
    //=============================================================================
    ArrayOf
    withoutField(const ArrayOf& value, const std::string& fieldToRemove)
    {
        if (!value.isStruct() || !value.isScalar()) {
            return emptyScalarStruct();
        }
        stringVector names;
        ArrayOfVector values;
        for (const std::string& field : value.fieldNames()) {
            if (field != fieldToRemove) {
                names.push_back(field);
                values.push_back(value.field(field));
            }
        }
        return ArrayOf::structScalar(names, values);
    }
    //=============================================================================
    ArrayOf
    groupValue(const ArrayOf& values, const std::wstring& group)
    {
        const std::string field = toFieldName(group);
        if (!values.isField(field)) {
            return emptyScalarStruct();
        }
        ArrayOf groupStruct = values.field(field);
        if (!groupStruct.isStruct() || !groupStruct.isScalar()) {
            Error(L"Nelson:preferences:invalidGroup", L"Invalid preferences group.");
        }
        return groupStruct;
    }
    //=============================================================================
    void
    setStructField(ArrayOf& value, const std::wstring& field, ArrayOf fieldValue)
    {
        const std::string utf8Field = toFieldName(field);
        value.setField(utf8Field, fieldValue);
    }
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
preferencesFilePath()
{
    std::wstring directory = preferencesDirectoryPath();
    if (directory.empty()) {
        Error(L"Nelson:preferences:pathNotFound", L"Cannot find Nelson preferences path.");
    }
    FileSystemWrapper::Path path(directory);
    if (!path.is_directory()) {
        std::string errorMessage;
        if (!FileSystemWrapper::Path::create_directories(path, errorMessage)) {
            Error(
                L"Nelson:preferences:cannotCreatePath", L"Cannot create Nelson preferences path.");
        }
    }
    path /= L"preferences.nh5";
    return path.generic_wstring();
}
//=============================================================================
std::wstring
extractScalarName(const ArrayOf& value, const std::wstring& argumentName)
{
    std::wstring name = value.asWideString();
    validateName(name, argumentName);
    return name;
}
//=============================================================================
PreferenceNames
extractPreferenceNames(const ArrayOf& value, const std::wstring& argumentName)
{
    PreferenceNames result;
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        result.names.push_back(value.asWideString());
        result.dimensions = Dimensions(1, 1);
        result.scalar = true;
    } else if (value.isCellArrayOfCharacterVectors() || value.isStringArray()) {
        result.names = value.asWideStringVector(false);
        result.dimensions = value.dimensions();
        result.scalar = false;
    } else {
        Error(L"Nelson:preferences:invalidNameType",
            L"%s must be a string, character vector, or cell array.", argumentName);
    }
    for (const std::wstring& name : result.names) {
        validateName(name, argumentName);
    }
    return result;
}
//=============================================================================
ArrayOf
getAllPreferences()
{
    std::lock_guard<std::mutex> lock(storeMutex);
    return loadStoreUnlocked().values;
}
//=============================================================================
bool
hasPreferenceGroup(const std::wstring& group)
{
    validateName(group, L"group");
    std::lock_guard<std::mutex> lock(storeMutex);
    return loadStoreUnlocked().values.isField(toFieldName(group));
}
//=============================================================================
std::vector<bool>
hasPreferences(const std::wstring& group, const PreferenceNames& preferences)
{
    validateName(group, L"group");
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    ArrayOf groupStruct = groupValue(store.values, group);
    std::vector<bool> result;
    result.reserve(preferences.names.size());
    for (const std::wstring& preference : preferences.names) {
        result.push_back(groupStruct.isField(toFieldName(preference)));
    }
    return result;
}
//=============================================================================
ArrayOfVector
getPreferences(const std::wstring& group, const PreferenceNames& preferences)
{
    validateName(group, L"group");
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    if (!store.values.isField(toFieldName(group))) {
        Error(
            L"Nelson:preferences:groupDoesNotExist", L"Preference group does not exist: %s", group);
    }
    ArrayOf groupStruct = groupValue(store.values, group);
    ArrayOfVector result;
    for (const std::wstring& preference : preferences.names) {
        const std::string field = toFieldName(preference);
        if (!groupStruct.isField(field)) {
            Error(L"Nelson:preferences:preferenceDoesNotExist", L"Preference does not exist: %s",
                preference);
        }
        result.push_back(groupStruct.field(field));
    }
    return result;
}
//=============================================================================
ArrayOf
getPreferenceOrDefault(
    const std::wstring& group, const std::wstring& preference, const ArrayOf& defaultValue)
{
    validateName(group, L"group");
    validateName(preference, L"preference");
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    ArrayOf groupStruct = groupValue(store.values, group);
    const std::string field = toFieldName(preference);
    if (groupStruct.isField(field)) {
        return groupStruct.field(field);
    }
    ArrayOf value = defaultValue;
    groupStruct.setField(field, value);
    setStructField(store.values, group, groupStruct);
    saveStoreUnlocked(store);
    return defaultValue;
}
//=============================================================================
bool
getPreferenceIfExists(const std::wstring& group, const std::wstring& preference, ArrayOf& value)
{
    validateName(group, L"group");
    validateName(preference, L"preference");
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    if (!store.values.isField(toFieldName(group))) {
        return false;
    }
    ArrayOf groupStruct = groupValue(store.values, group);
    const std::string field = toFieldName(preference);
    if (!groupStruct.isField(field)) {
        return false;
    }
    value = groupStruct.field(field);
    return true;
}
//=============================================================================
void
setPreferences(const std::wstring& group, const PreferenceNames& preferences,
    const ArrayOfVector& values, bool failIfExists)
{
    validateName(group, L"group");
    if (preferences.names.size() != values.size()) {
        Error(L"Nelson:preferences:countMismatch", L"Number of preferences and values must match.");
    }
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    ArrayOf groupStruct = groupValue(store.values, group);
    for (size_t k = 0; k < preferences.names.size(); ++k) {
        const std::string field = toFieldName(preferences.names[k]);
        if (failIfExists && groupStruct.isField(field)) {
            Error(L"Nelson:preferences:preferenceAlreadyExists", L"Preference already exists: %s",
                preferences.names[k]);
        }
        ArrayOf value = values[k];
        groupStruct.setField(field, value);
    }
    setStructField(store.values, group, groupStruct);
    saveStoreUnlocked(store);
}
//=============================================================================
void
removePreferenceGroup(const std::wstring& group)
{
    validateName(group, L"group");
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    const std::string field = toFieldName(group);
    if (!store.values.isField(field)) {
        Error(
            L"Nelson:preferences:groupDoesNotExist", L"Preference group does not exist: %s", group);
    }
    store.values = withoutField(store.values, field);
    store.registry = withoutField(store.registry, field);
    saveStoreUnlocked(store);
}
//=============================================================================
void
removePreferences(const std::wstring& group, const PreferenceNames& preferences)
{
    validateName(group, L"group");
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    if (!store.values.isField(toFieldName(group))) {
        Error(
            L"Nelson:preferences:groupDoesNotExist", L"Preference group does not exist: %s", group);
    }
    ArrayOf groupStruct = groupValue(store.values, group);
    for (const std::wstring& preference : preferences.names) {
        const std::string field = toFieldName(preference);
        if (!groupStruct.isField(field)) {
            Error(L"Nelson:preferences:preferenceDoesNotExist", L"Preference does not exist: %s",
                preference);
        }
    }
    for (const std::wstring& preference : preferences.names) {
        groupStruct = withoutField(groupStruct, toFieldName(preference));
    }
    setStructField(store.values, group, groupStruct);
    if (store.registry.isField(toFieldName(group))) {
        ArrayOf registryGroup = groupValue(store.registry, group);
        for (const std::wstring& preference : preferences.names) {
            registryGroup = withoutField(registryGroup, toFieldName(preference));
        }
        setStructField(store.registry, group, registryGroup);
    }
    saveStoreUnlocked(store);
}
//=============================================================================
void
registerUiPreference(const std::wstring& group, const std::wstring& preference)
{
    validateName(group, L"group");
    validateName(preference, L"preference");
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    ArrayOf registryGroup = groupValue(store.registry, group);
    ArrayOf marker = ArrayOf::logicalScalar(true);
    registryGroup.setField(toFieldName(preference), marker);
    setStructField(store.registry, group, registryGroup);
    saveStoreUnlocked(store);
}
//=============================================================================
ArrayOf
resetRegisteredUiPreferences()
{
    std::lock_guard<std::mutex> lock(storeMutex);
    Store store = loadStoreUnlocked();
    for (const std::string& groupField : store.registry.fieldNames()) {
        ArrayOf registryGroup = groupValue(store.registry, utf8_to_wstring(groupField));
        ArrayOf valueGroup = groupValue(store.values, utf8_to_wstring(groupField));
        for (const std::string& preferenceField : registryGroup.fieldNames()) {
            ArrayOf ask = ArrayOf::characterArray("ask");
            valueGroup.setField(preferenceField, ask);
        }
        ArrayOf copy = valueGroup;
        store.values.setField(groupField, copy);
    }
    saveStoreUnlocked(store);
    return store.values;
}
//=============================================================================
} // namespace Nelson::Preferences
//=============================================================================
