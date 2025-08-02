//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FindCommonType.hpp"
#include "Error.hpp"
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
//=============================================================================
bool
FindCommonType(const ArrayOfVector& argIn, NelsonType& commonType, bool& isSparse, bool& isComplex,
    std::string& typeName)
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
            typeName = argIn[0].getClassType();
            commonType = NLS_CLASS_ARRAY;
            isSparse = false;
            isComplex = false;
            return true;
        }
        if (argIn[0].isHandle()) {
            if (argIn[0].isEmpty()) {
                typeName = NLS_HANDLE_STR;
            } else {
                typeName = argIn[0].getHandleCategory();
            }
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

        commonType = argIn[0].getDataClass();
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
        if (argIn[0].getDataClass() == argIn[1].getDataClass()) {
            if (argIn[0].isHandle()) {
                if (argIn[0].isEmpty()) {
                    typeName = NLS_HANDLE_STR;
                } else {
                    typeName = argIn[0].getHandleCategory();
                }
                commonType = NLS_HANDLE;
                return true;
            }
            if (argIn[0].isClassType()) {
                typeName = argIn[0].getClassType();
                commonType = NLS_CLASS_ARRAY;
                isSparse = false;
                isComplex = false;
                return true;
            }

            commonType = argIn[0].getDataClass();
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
            commonType = argIn[i].getDataClass();
        } else {
            commonType = getConcatenateCommonType(commonType, argIn[i].getDataClass());
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
        if (!haveString && argIn[i].isStringArray()) {
            haveString = true;
            break;
        }
    }
    isComplex = !realOnly;
    if (haveString) {
        typeName = NLS_STRING_ARRAY_STR;
        commonType = NLS_STRING_ARRAY;
    } else if (haveCell) {
        typeName = NLS_CELL_ARRAY_STR;
        commonType = NLS_CELL_ARRAY;
    } else if (haveObject) {
        // currently, first object type will be used
        // superiorto function need to be added to manage full compatibility
        for (size_t k = 0; k < argIn.size(); k++) {
            NelsonType currentType = argIn[k].getDataClass();
            if (currentType == NLS_HANDLE) {
                if (argIn[k].isEmpty()) {
                    typeName = NLS_HANDLE_STR;
                } else {
                    typeName = argIn[k].getHandleCategory();
                }
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
                typeName = argIn[k].getClassType();
                return true;
            }
        }
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
    if (haveSparse && (commonType == NLS_DOUBLE || commonType == NLS_LOGICAL)) {
        typeName = NLS_SPARSE_STR + ClassToString(commonType);
    }
    if (isSparse) {
        return (
            commonType == NLS_DOUBLE || commonType == NLS_DCOMPLEX || commonType == NLS_LOGICAL);
    }
    return (NLS_UNKNOWN != commonType);
}
//=============================================================================
static bool
isObject(const ArrayOf& A)
{
    return A.getDataClass() >= NLS_CLASS_ARRAY;
}
//=============================================================================
static NelsonType
getConcatenateCommonType(NelsonType type1, NelsonType type2)
{
    if (type1 == type2) {
        return type1;
    } else if (type1 == NLS_HANDLE || type2 == NLS_HANDLE) {
        return NLS_HANDLE;
    } else if (type1 == NLS_GO_HANDLE || type2 == NLS_GO_HANDLE) {
        return NLS_GO_HANDLE;
    } else if (type1 == NLS_CELL_ARRAY || type2 == NLS_CELL_ARRAY) {
        return NLS_CELL_ARRAY;
    } else if (type1 == NLS_STRUCT_ARRAY || type2 == NLS_STRUCT_ARRAY) {
        return NLS_STRUCT_ARRAY;
    } else if (type1 == NLS_MISSING_ARRAY || type2 == NLS_MISSING_ARRAY) {
        if (type1 == NLS_MISSING_ARRAY) {
            return type2;
        }
        return type1;
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
bool
FindCommonTypeRelationalOperators(const ArrayOfVector& args, NelsonType& commonType, bool& isSparse,
    bool& isComplex, std::string& typeName)
{
    commonType = NLS_UNKNOWN;
    typeName = NLS_UNKNOWN_STR;
    ArrayOf A(args[0]);
    ArrayOf B(args[1]);
    NelsonType Aclass = A.getDataClass();
    NelsonType Bclass = B.getDataClass();
    isSparse = A.isSparse() || B.isSparse();
    isComplex = A.isComplex() || B.isComplex();

    if ((Aclass == Bclass) && (Aclass <= NLS_CHAR)) {
        commonType = Aclass;
        typeName = ClassToString(commonType);
        if (isSparse) {
            typeName = NLS_SPARSE_STR + typeName;
        }
        return true;
    }
    // The output class is now the dominant class remaining:
    if (isObject(A)) {
        if (Aclass == NLS_HANDLE) {
            if (A.isEmpty()) {
                typeName = NLS_HANDLE_STR;
            } else {
                typeName = A.getHandleCategory();
            }
            commonType = NLS_HANDLE;
            return true;
        }
        if (Aclass == NLS_GO_HANDLE) {
            commonType = NLS_GO_HANDLE;
            typeName = NLS_GO_HANDLE_STR;
            return true;
        }
        if (Aclass == NLS_CLASS_ARRAY) {
            commonType = NLS_CLASS_ARRAY;
            typeName = A.getClassType();
            return true;
        }
        return false;
    }
    if (isObject(B)) {
        if (Bclass == NLS_HANDLE) {
            if (B.isEmpty()) {
                typeName = NLS_HANDLE_STR;
            } else {
                typeName = B.getHandleCategory();
            }
            commonType = NLS_HANDLE;
            return true;
        }
        if (Bclass == NLS_GO_HANDLE) {
            commonType = NLS_GO_HANDLE;
            typeName = NLS_GO_HANDLE_STR;
            return true;
        }
        if (Bclass == NLS_CLASS_ARRAY) {
            commonType = NLS_CLASS_ARRAY;
            typeName = B.getClassType();
            return true;
        }
        return false;
    }

    if (A.isIntegerType() && B.isIntegerType()) {
        if (Aclass > Bclass) {
            commonType = Aclass;
        } else {
            commonType = Bclass;
        }
        typeName = ClassToString(commonType);
        return true;
    }

    // An integer or double mixed with a complex is promoted to a dcomplex type
    if ((Aclass == NLS_SCOMPLEX) && ((Bclass == NLS_DOUBLE) || (Bclass < NLS_SINGLE))) {
        Bclass = NLS_DCOMPLEX;
    }
    if ((Bclass == NLS_SCOMPLEX) && ((Aclass == NLS_DOUBLE) || (Aclass < NLS_SINGLE))) {
        Aclass = NLS_DCOMPLEX;
    }

    bool isDoubleTypeA = Aclass == NLS_DOUBLE || Aclass == NLS_DCOMPLEX;
    bool isDoubleTypeB = Bclass == NLS_DOUBLE || Bclass == NLS_DCOMPLEX;

    if (isDoubleTypeA) {
        commonType = NLS_DOUBLE;
        typeName = NLS_DOUBLE_STR;
        return true;
    }
    if (isDoubleTypeB) {
        commonType = NLS_DOUBLE;
        typeName = NLS_DOUBLE_STR;
        return true;
    }

    bool isSingleTypeA = Aclass == NLS_SINGLE || Aclass == NLS_SCOMPLEX;
    bool isSingleTypeB = Bclass == NLS_SINGLE || Bclass == NLS_SCOMPLEX;

    if (isSingleTypeA) {
        commonType = NLS_SINGLE;
        typeName = NLS_SINGLE_STR;
        return true;
    }
    if (isSingleTypeB) {
        commonType = NLS_SINGLE;
        typeName = NLS_SINGLE_STR;
        return true;
    }

    if (Aclass > Bclass) {
        commonType = Aclass;
        if (Aclass < NLS_CLASS_ARRAY) {
            typeName = ClassToString(Aclass);
        } else {
            typeName = ClassName(A);
        }
    } else {
        commonType = Bclass;
        if (Bclass < NLS_CLASS_ARRAY) {
            typeName = ClassToString(Bclass);
        } else {
            typeName = ClassName(B);
        }
    }
    return (commonType != NLS_UNKNOWN);
}
//=============================================================================
}
