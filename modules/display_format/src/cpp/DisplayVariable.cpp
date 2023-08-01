//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "DisplayVariable.hpp"
#include "DisplayVariableHelpers.hpp"
#include "characters_encoding.hpp"
#include "DisplayCell.hpp"
#include "DisplayStruct.hpp"
#include "DisplayClass.hpp"
#include "DisplayString.hpp"
#include "DisplayChar.hpp"
#include "DisplayLogical.hpp"
#include "DisplayInteger.hpp"
#include "DisplayDouble.hpp"
#include "DisplayDoubleComplex.hpp"
#include "DisplaySingle.hpp"
#include "DisplaySingleComplex.hpp"
#include "DisplaySparseDouble.hpp"
#include "DisplaySparseDoubleComplex.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
DisplayVariable(size_t evaluatorID, Interface* io, const ArrayOf& A, const std::wstring& name,
    bool asDisp, bool& needToOverload)
{
    if (io == nullptr) {
        return;
    }
    switch (A.getDataClass()) {
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        DisplayInteger(evaluatorID, io, A, name, asDisp);
        needToOverload = false;
    } break;
    case NLS_LOGICAL: {
        DisplayLogical(evaluatorID, io, A, name, asDisp);
        needToOverload = false;
    } break;
    case NLS_CHAR: {
        DisplayChar(evaluatorID, io, A, name, asDisp);
        needToOverload = false;
    } break;
    case NLS_STRING_ARRAY: {
        DisplayString(evaluatorID, io, A, name, asDisp);
        needToOverload = false;
    } break;
    case NLS_CLASS_ARRAY: {
        bool needToHideHeader = false;
        if (asDisp) {
            needToHideHeader = (A.isEmpty() || A.isScalar());
        } else {
            needToHideHeader = false;
        }
        DisplayClass(io, A, name, needToHideHeader);
        needToOverload = false;
    } break;
    case NLS_STRUCT_ARRAY: {
        bool needToHideHeader = false;
        if (asDisp) {
            needToHideHeader = (A.isEmpty() || A.isScalar());
        } else {
            needToHideHeader = false;
        }
        DisplayStruct(io, A, name, needToHideHeader);
        needToOverload = false;
    } break;
    case NLS_CELL_ARRAY: {
        DisplayCell(evaluatorID, io, A, name, asDisp);
        needToOverload = false;
    } break;
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            DisplaySparseDouble(evaluatorID, io, A, name, asDisp);
        } else {
            DisplayDouble(evaluatorID, io, A, name, asDisp);
        }
        needToOverload = false;
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            DisplaySparseDoubleComplex(evaluatorID, io, A, name, asDisp);
        } else {
            DisplayDoubleComplex(evaluatorID, io, A, name, asDisp);
        }
        needToOverload = false;
    } break;
    case NLS_SINGLE: {
        DisplaySingle(evaluatorID, io, A, name, asDisp);
        needToOverload = false;
    } break;
    case NLS_SCOMPLEX: {
        DisplaySingleComplex(evaluatorID, io, A, name, asDisp);
        needToOverload = false;
    } break;

    default: {
        needToOverload = true;
    } break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
