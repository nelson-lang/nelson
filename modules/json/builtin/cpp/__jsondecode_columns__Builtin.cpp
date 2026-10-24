//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__jsondecode_columns__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "JsonDecodeColumns.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static JsonTableLocation
locationArgument(const ArrayOf& arg);
//=============================================================================
ArrayOfVector
Nelson::JsonGateway::__jsondecode_columns__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    // [ok, names, columns, selector] = __jsondecode_columns__(filename)
    // [...] = __jsondecode_columns__(filename, 'pointer', tokens)
    // [...] = __jsondecode_columns__(filename, 'name', key)
    // [...] = __jsondecode_columns__(filename, 'auto')
    // decodes the table node of a JSON file, an array of flat objects, into
    // n x 1 columns (see JsonDecodeColumns.hpp); ok is false for any other
    // document.
    ArrayOfVector retval;
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 4);
    std::wstring filename = argIn[0].asWideString();
    JsonTableLocation location = JsonTableLocation::Root;
    stringVector keys;
    if (argIn.size() > 1) {
        location = locationArgument(argIn[1]);
    }
    if (argIn.size() > 2) {
        keys = argIn[2].isCell() ? argIn[2].asUtf8StringVector()
                                 : stringVector { argIn[2].asUtf8String() };
    }
    std::string selector;
    stringVector names;
    ArrayOfVector columns;
    bool ok = jsonDecodeColumnsFile(filename, location, keys, selector, names, columns);
    Dimensions dims(1, columns.size());
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, columns.size(), stringVector(), false));
    for (size_t k = 0; k < columns.size(); ++k) {
        elements[k] = columns[k];
    }
    retval << ArrayOf::logicalScalar(ok);
    retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(names);
    retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
    retval << ArrayOf::characterArray(selector);
    return retval;
}
//=============================================================================
static JsonTableLocation
locationArgument(const ArrayOf& arg)
{
    std::wstring name = arg.asWideString();
    if (name == L"root") {
        return JsonTableLocation::Root;
    }
    if (name == L"pointer") {
        return JsonTableLocation::Pointer;
    }
    if (name == L"name") {
        return JsonTableLocation::NodeName;
    }
    if (name == L"auto") {
        return JsonTableLocation::Automatic;
    }
    Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (2));
    return JsonTableLocation::Root;
}
//=============================================================================
