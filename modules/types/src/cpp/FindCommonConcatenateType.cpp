//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "FindCommonConcatenateType.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ClassToString.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isObject(const ArrayOf& A);
static NelsonType
getConcatenateCommonType(NelsonType type1, NelsonType type2);
template <class T>
static bool
imaginaryPartIsNonZero(const void* data, indexType count, bool isUnsigned);
//=============================================================================
bool
FindCommonConcatenateType(const ArrayOfVector& argIn, NelsonType& commonType, bool& isSparse,
    bool& isComplex, std::string& typeName)
{
    commonType = NLS_UNKNOWN;
    typeName = NLS_UNKNOWN_STR;
    isSparse = false;
    isComplex = false;

    bool charactersOnly = true;
    bool realOnly = true;
    bool complexOnly = true;
    bool haveSparse = false;
    bool haveCell = false;
    bool haveObject = false;
    bool haveString = false;

    if (argIn.size() == 0) {
        return false;
    }
    if (argIn.size() == 1) {
        if (argIn[0].isClassType()) {
            typeName = argIn[0].classType();
            commonType = argIn[0].dataClass();
            isSparse = false;
            isComplex = false;
            return true;
        }
        if (argIn[0].isHandle()) {
            typeName = argIn[0].handleCategory();
            commonType = NLS_HANDLE;
            isSparse = false;
            isComplex = false;
            return true;
        }
        if (argIn[0].isGraphicsObject()) {
            typeName = NLS_GO_HANDLE_STR;
            commonType = NLS_GO_HANDLE;
            isSparse = false;
            isComplex = false;
            return true;
        }

        commonType = argIn[0].dataClass();
        if (commonType == NLS_DCOMPLEX) {
            commonType = NLS_DOUBLE;
            isComplex = true;
        }
        if (commonType == NLS_SCOMPLEX) {
            commonType = NLS_SINGLE;
            isComplex = true;
        }
        typeName = ClassToString(commonType);
        if (argIn[0].isSparse()) {
            isSparse = true;
            typeName = NLS_SPARSE_STR + typeName;
        }
        return true;
    }
    if (argIn.size() == 2) {
        if (argIn[0].dataClass() == argIn[1].dataClass()) {
            if (argIn[0].isHandle()) {
                typeName = argIn[0].handleCategory();
                commonType = NLS_HANDLE;
                return true;
            }
            if (argIn[0].isClassType()) {
                typeName = argIn[0].classType();
                commonType = argIn[0].dataClass();
                return true;
            }
            commonType = argIn[0].dataClass();
            if (commonType == NLS_DCOMPLEX) {
                commonType = NLS_DOUBLE;
                isComplex = true;
            }
            if (commonType == NLS_SCOMPLEX) {
                commonType = NLS_SINGLE;
                isComplex = true;
            }
            typeName = ClassToString(commonType);
            if (argIn[0].isSparse() || argIn[1].isSparse()) {
                isSparse = true;
                typeName = NLS_SPARSE_STR + typeName;
            }
            return true;
        }
    }
    for (size_t i = 0; i < argIn.size(); i++) {
        if (i == 0) {
            commonType = argIn[i].dataClass();
        } else {
            commonType = getConcatenateCommonType(commonType, argIn[i].dataClass());
        }

        if (charactersOnly && !argIn[i].isCharacterArray()) {
            charactersOnly = false;
        }
        if (realOnly && !argIn[i].isReal()) {
            realOnly = false;
        }
        if (complexOnly && !(argIn[i].isComplex() || argIn[i].isReal())) {
            complexOnly = false;
        }
        if (!haveSparse && argIn[i].isSparse()) {
            haveSparse = true;
            isSparse = true;
        }
        if (!haveCell && argIn[i].isCell()) {
            haveCell = true;
        }
        if (!haveObject && isObject(argIn[i])) {
            haveObject = true;
            break;
        }
        // An object anywhere in the list decides the dispatch, even after a
        // string or cell operand (["a" obj] calls the class concatenation).
        if (!haveString && argIn[i].isStringArray()) {
            haveString = true;
        }
    }
    isComplex = !realOnly;
    if (haveObject) {
        // currently, first object type will be used
        // superiorto function need to be added to manage full compatibility
        for (size_t k = 0; k < argIn.size(); k++) {
            NelsonType currentType = argIn[k].dataClass();
            if (currentType == NLS_HANDLE) {
                typeName = argIn[k].handleCategory();
                commonType = NLS_HANDLE;
                return true;
            }
            if (currentType == NLS_GO_HANDLE) {
                commonType = NLS_GO_HANDLE;
                typeName = NLS_GO_HANDLE_STR;
                return true;
            }
            if (currentType == NLS_CLASS_ARRAY) {
                commonType = NLS_CLASS_ARRAY;
                typeName = argIn[k].classType();
                return true;
            }
            if (currentType == NLS_CLASSDEF_ARRAY) {
                commonType = NLS_CLASSDEF_ARRAY;
                typeName = argIn[k].classType();
                return true;
            }
        }
    } else if (haveString) {
        typeName = NLS_STRING_ARRAY_STR;
        commonType = NLS_STRING_ARRAY;
    } else if (haveCell && commonType != NLS_FUNCTION_HANDLE && commonType != NLS_STRUCT_ARRAY) {
        typeName = NLS_CELL_ARRAY_STR;
        commonType = NLS_CELL_ARRAY;
    }
    if (commonType == NLS_DCOMPLEX) {
        commonType = NLS_DOUBLE;
        isComplex = true;
    }
    if (commonType == NLS_SCOMPLEX) {
        commonType = NLS_SINGLE;
        isComplex = true;
    }
    typeName = ClassToString(commonType);
    if (haveSparse
        && (commonType == NLS_DOUBLE || commonType == NLS_SINGLE || commonType == NLS_LOGICAL)) {
        typeName = NLS_SPARSE_STR + ClassToString(commonType);
    }
    if (isSparse) {
        return commonType == NLS_DOUBLE || commonType == NLS_DCOMPLEX || commonType == NLS_SINGLE
            || commonType == NLS_SCOMPLEX || commonType == NLS_LOGICAL;
    }
    return (NLS_UNKNOWN != commonType);
}
//=============================================================================
void
prepareMissingConcatenation(ArrayOfVector& argIn, NelsonType commonType)
{
    switch (commonType) {
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX: {
        // A complex operand, even empty, makes the missing values complex:
        // the empty operand is dropped by the kernel, the missing one is not.
        bool haveMissing = false;
        bool haveComplex = false;
        for (const auto& value : argIn) {
            haveMissing = haveMissing || value.isMissing();
            haveComplex = haveComplex || value.isComplex();
        }
        if (!haveMissing) {
            return;
        }
        const bool isSingle = (commonType == NLS_SINGLE || commonType == NLS_SCOMPLEX);
        NelsonType missingType = isSingle ? NLS_SINGLE : NLS_DOUBLE;
        if (haveComplex) {
            missingType = isSingle ? NLS_SCOMPLEX : NLS_DCOMPLEX;
        }
        for (auto& value : argIn) {
            if (value.isMissing()) {
                value.promoteType(missingType);
            }
        }
        return;
    } break;
    case NLS_MISSING_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_CLASSDEF_ARRAY:
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN: {
        return;
    } break;
    default: {
    } break;
    }
    for (const auto& value : argIn) {
        if (value.isMissing()) {
            RaiseError(formatI18nMessage(_("Conversion to %s from missing is not possible."),
                           ClassToString(commonType)),
                "Nelson:invalidConversion");
        }
    }
}
//=============================================================================
void
checkComplexIntegerConcatenation(const ArrayOfVector& argIn, NelsonType commonType)
{
    if (!IS_INTEGER_TYPE(commonType)) {
        return;
    }
    const bool isUnsigned = IS_UNSIGNED_INTEGER_TYPE(commonType);
    for (const auto& value : argIn) {
        if (!value.isComplex() || value.isEmpty() || value.isSparse()) {
            continue;
        }
        const indexType count = value.elementCount();
        const bool hasImaginaryPart = (value.dataClass() == NLS_SCOMPLEX)
            ? imaginaryPartIsNonZero<single>(value.dataPointer(), count, isUnsigned)
            : imaginaryPartIsNonZero<double>(value.dataPointer(), count, isUnsigned);
        if (hasImaginaryPart) {
            Error("Nelson:concatenation:complexIntegerUnsupported",
                "Complex integers are not supported: an integer cannot be concatenated with a "
                "value that has a nonzero imaginary part.");
        }
    }
}
//=============================================================================
template <class T>
static bool
imaginaryPartIsNonZero(const void* data, indexType count, bool isUnsigned)
{
    const T* values = static_cast<const T*>(data);
    for (indexType k = 0; k < count; ++k) {
        const T imaginary = values[2 * k + 1];
        if (std::isnan(imaginary)) {
            continue;
        }
        const T rounded = std::round(imaginary);
        if (isUnsigned ? (rounded > 0) : (rounded != 0)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
isObject(const ArrayOf& A)
{
    return A.dataClass() >= NLS_CLASS_ARRAY;
}
//=============================================================================
static NelsonType
getConcatenateCommonType(NelsonType type1, NelsonType type2)
{
    if (type1 == type2) {
        return type1;
    } else if (type1 == NLS_MISSING_ARRAY || type2 == NLS_MISSING_ARRAY) {
        if (type1 == NLS_MISSING_ARRAY) {
            return type2;
        }
        return type1;
    } else if (type1 == NLS_HANDLE || type2 == NLS_HANDLE) {
        return NLS_HANDLE;
    } else if (type1 == NLS_GO_HANDLE || type2 == NLS_GO_HANDLE) {
        return NLS_GO_HANDLE;
    } else if ((type1 == NLS_FUNCTION_HANDLE && type2 == NLS_CELL_ARRAY)
        || (type1 == NLS_CELL_ARRAY && type2 == NLS_FUNCTION_HANDLE)) {
        // [@f, {}]: the handle wins, a non-empty cell cannot be converted
        return NLS_FUNCTION_HANDLE;
    } else if (type1 == NLS_STRUCT_ARRAY && type2 == NLS_CELL_ARRAY) {
        // [s, {}]: a struct before any cell wins, a non-empty cell cannot be converted
        return NLS_STRUCT_ARRAY;
    } else if (type1 == NLS_CELL_ARRAY || type2 == NLS_CELL_ARRAY) {
        return NLS_CELL_ARRAY;
    } else if (type1 == NLS_CLASSDEF_ARRAY || type2 == NLS_CLASSDEF_ARRAY) {
        return NLS_CLASSDEF_ARRAY;
    } else if (type1 == NLS_FUNCTION_HANDLE || type2 == NLS_FUNCTION_HANDLE) {
        return NLS_FUNCTION_HANDLE;
    } else if (type1 == NLS_STRUCT_ARRAY || type2 == NLS_STRUCT_ARRAY) {
        return NLS_STRUCT_ARRAY;
    } else {
        bool type1IsInteger = IS_INTEGER_TYPE(type1);
        bool type2IsInteger = IS_INTEGER_TYPE(type2);
        bool type1IsCharacter = (type1 == NLS_CHAR);
        bool type2IsCharacter = (type2 == NLS_CHAR);

        bool type1IsDouble = (type1 == NLS_DOUBLE || type1 == NLS_DCOMPLEX);
        bool type2IsDouble = (type2 == NLS_DOUBLE || type2 == NLS_DCOMPLEX);

        bool type1IsSingle = (type1 == NLS_SINGLE || type1 == NLS_SCOMPLEX);
        bool type2IsSingle = (type2 == NLS_SINGLE || type2 == NLS_SCOMPLEX);

        bool type1IsLogical = (type1 == NLS_LOGICAL);
        bool type2IsLogical = (type2 == NLS_LOGICAL);

        bool type1IsBasicType = (type1IsInteger || type1IsCharacter || type1IsDouble
            || type1IsSingle || type1IsLogical);

        bool type2IsBasicType = (type2IsInteger || type2IsCharacter || type2IsDouble
            || type2IsSingle || type2IsLogical);

        if (type1IsCharacter && type2IsBasicType) {
            return type1;
        } else if (type2IsCharacter && type1IsBasicType) {
            return type2;
        } else if (type1IsInteger && type2IsBasicType) {
            return type1;
        } else if (type2IsInteger && type1IsBasicType) {
            return type2;
        } else if (type1IsSingle && type2IsBasicType) {
            return type1;
        } else if (type2IsSingle && type1IsBasicType) {
            return type2;
        } else if (type1IsDouble && type2IsBasicType) {
            return type1;
        } else if (type2IsDouble && type1IsBasicType) {
            return type2;
        } else if (type1IsLogical && type2IsLogical) {
            return type1;
        }
    }
    return NLS_UNKNOWN;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
