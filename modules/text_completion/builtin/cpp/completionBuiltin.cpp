//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "completionBuiltin.hpp"
#include "CompleterHelper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define COMPLETION_FIELD_PREFIX L"prefix"
#define COMPLETION_FIELD_SHOWPOPUP L"showpopup"
#define COMPLETION_FIELD_FILES L"files"
#define COMPLETION_FIELD_BUILTIN L"builtin"
#define COMPLETION_FIELD_MACROS L"macros"
#define COMPLETION_FIELD_VARIABLES L"variables"
#define COMPLETION_FIELD_FIELDS L"fields"
#define COMPLETION_FIELD_PROPERTIES L"properties"
#define COMPLETION_FIELD_METHODS L"methods"
//=============================================================================
ArrayOfVector
Nelson::TextCompletionGateway::completionBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    std::wstring line = argIn[0].asWideString();
    std::wstring completionPrefix;
    wstringVector files;
    wstringVector builtin;
    wstringVector macros;
    wstringVector variables;
    wstringVector fields;
    wstringVector properties;
    wstringVector methods;
    bool showpopup = computeCompletion(eval, line, completionPrefix, files, builtin, macros,
        variables, fields, properties, methods);

    wstringVector fieldnames
        = { COMPLETION_FIELD_PREFIX, COMPLETION_FIELD_SHOWPOPUP, COMPLETION_FIELD_FILES,
              COMPLETION_FIELD_BUILTIN, COMPLETION_FIELD_MACROS, COMPLETION_FIELD_VARIABLES,
              COMPLETION_FIELD_FIELDS, COMPLETION_FIELD_PROPERTIES, COMPLETION_FIELD_METHODS };
    ArrayOfVector values;
    values << ArrayOf::characterArray(completionPrefix);
    values << ArrayOf::logicalScalar(showpopup);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(files);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(builtin);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(macros);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(variables);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(fields);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(properties);
    values << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(methods);
    retval << ArrayOf::structScalar(fieldnames, values);
    return retval;
}
//=============================================================================
