//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "dictionaryStoreBuiltin.hpp"
#include "DictionaryStoreObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <memory>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
bool
isTextScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
std::string
actionOf(const ArrayOf& value)
{
    if (!isTextScalar(value)) {
        Error(L"Nelson:dictionary:invalidStoreAction", L"Dictionary store action must be text.");
    }
    return value.asUtf8String();
}
//=============================================================================
DictionaryStoreObject*
getStore(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()
        || value.handleCategory() != NLS_HANDLE_DICTIONARY_STORE_CATEGORY_STR) {
        Error(L"Nelson:dictionary:storeExpected", L"Native dictionary store expected.");
    }
    auto* object = dynamic_cast<DictionaryStoreObject*>(value.asHandleScalar());
    if (object == nullptr) {
        Error(L"Nelson:dictionary:storeExpected", L"Native dictionary store expected.");
    }
    return object;
}
//=============================================================================
// A key/value argument is always passed from the .m layer as a cell list (one
// cell per entry), so the native side stays type-agnostic.
std::vector<ArrayOf>
cellToVector(const ArrayOf& value)
{
    std::vector<ArrayOf> result;
    if (!value.isCell()) {
        result.push_back(value);
        return result;
    }
    const indexType n = value.elementCount();
    if (n == 0) {
        return result;
    }
    const ArrayOf* elements = static_cast<const ArrayOf*>(value.dataPointer());
    result.reserve((size_t)n);
    for (indexType k = 0; k < n; ++k) {
        result.push_back(elements[k]);
    }
    return result;
}
//=============================================================================
ArrayOf
makeCellRow(const std::vector<ArrayOf>& values)
{
    Dimensions dims(1, (indexType)values.size());
    if (values.empty()) {
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }
    auto* elements = new ArrayOf[values.size()];
    for (size_t k = 0; k < values.size(); ++k) {
        elements[k] = values[k];
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
ArrayOf
makeStoreHandle(DictionaryStoreObject* object)
{
    return ArrayOf::handle(object);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::DictionaryGateway::dictionaryStoreBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);

    // Creation form: __dictionary_store__('new')
    if (argIn.size() == 1 && isTextScalar(argIn[0]) && argIn[0].asUtf8String() == "new") {
        return ArrayOfVector { makeStoreHandle(new DictionaryStoreObject()) };
    }

    nargincheck(argIn, 2, 4);
    DictionaryStoreObject* store = getStore(argIn[0]);
    const std::string action = actionOf(argIn[1]);

    if (action == "count") {
        return ArrayOfVector { ArrayOf::doubleScalar((double)store->count()) };
    }
    if (action == "keys") {
        return ArrayOfVector { makeCellRow(store->keysInOrder()) };
    }
    if (action == "values") {
        return ArrayOfVector { makeCellRow(store->valuesInOrder()) };
    }
    if (action == "set") {
        nargincheck(argIn, 4, 4);
        std::vector<ArrayOf> keys = cellToVector(argIn[2]);
        std::vector<ArrayOf> values = cellToVector(argIn[3]);
        if (keys.size() != values.size()) {
            Error(L"Nelson:dictionary:keyValueCountMismatch",
                L"The number of keys and values must match.");
        }
        return ArrayOfVector { makeStoreHandle(store->withSet(eval, keys, values)) };
    }
    if (action == "remove") {
        nargincheck(argIn, 3, 3);
        std::vector<ArrayOf> keys = cellToVector(argIn[2]);
        return ArrayOfVector { makeStoreHandle(store->withRemoved(eval, keys)) };
    }
    if (action == "get") {
        nargincheck(argIn, 3, 3);
        std::vector<ArrayOf> keys = cellToVector(argIn[2]);
        std::vector<ArrayOf> out;
        out.reserve(keys.size());
        ArrayOf value;
        for (const auto& key : keys) {
            if (!store->lookup(eval, key, value)) {
                Error(L"Nelson:dictionary:unknownKey", L"Unknown key");
            }
            out.push_back(value);
        }
        return ArrayOfVector { makeCellRow(out) };
    }
    if (action == "iskey") {
        nargincheck(argIn, 3, 3);
        std::vector<ArrayOf> keys = cellToVector(argIn[2]);
        Dimensions dims(1, (indexType)keys.size());
        logical* data = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, (indexType)keys.size(), stringVector(), false));
        for (size_t k = 0; k < keys.size(); ++k) {
            data[k] = store->isKey(eval, keys[k]) ? 1 : 0;
        }
        return ArrayOfVector { ArrayOf(NLS_LOGICAL, dims, data) };
    }
    Error(L"Nelson:dictionary:unsupportedStoreAction", L"Unsupported dictionary store action.");
    return {};
}
//=============================================================================
