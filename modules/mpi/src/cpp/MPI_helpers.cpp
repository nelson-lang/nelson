//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <mpi.h>
#include "MPI_helpers.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleManager.hpp"
#include "MPI_CommHandleObject.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "SparseConstructors.hpp"
#include "SparseToIJV.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifndef MPI_MAX_LIBRARY_VERSION_STRING
#define MPI_MAX_LIBRARY_VERSION_STRING 64
#endif
//=============================================================================
#ifndef MPI_MAX_OBJECT_NAME
#define MPI_MAX_OBJECT_NAME 128
#endif
//=============================================================================
static MPI_Errhandler errhdl;
//=============================================================================
void
MPIErrorHandler(MPI_Comm* comm, int* errorcode, ...)
{
    char buffer[MPI_MAX_ERROR_STRING];
    int resultlen = 0;
    MPI_Error_string(*errorcode, buffer, &resultlen);
    buffer[resultlen] = 0;
    Error(buffer);
}
//=============================================================================
int
initializeMPI()
{
    int flag = 1;
    MPI_Initialized(&flag);
    if (flag) {
        return flag;
    }
    MPI_Init(nullptr, nullptr);
    MPI_Comm_create_errhandler(MPIErrorHandler, &errhdl);
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, errhdl);
    MPI_Initialized(&flag);
    return flag;
}
//=============================================================================
static NelsonType
IntToClass(int code)
{
    return static_cast<NelsonType>(code / 1000);
}
//=============================================================================
static int
ClassToInt(NelsonType dataClass)
{
    return (dataClass * 1000);
}
//=============================================================================
void
packMPI(ArrayOf& A, void* buffer, int bufsize, int* packpos, MPI_Comm comm)
{
    NelsonType dataClass(A.getDataClass());
    int idclass = ClassToInt(dataClass);
    MPI_Pack(&idclass, 1, MPI_INT, buffer, bufsize, packpos, comm);
    int issparse = static_cast<int>(A.isSparse());
    MPI_Pack(&issparse, 1, MPI_INT, buffer, bufsize, packpos, comm);
    int dimlength = static_cast<int>(A.nDims());
    MPI_Pack(&dimlength, 1, MPI_INT, buffer, bufsize, packpos, comm);
    for (int j = 0; j < dimlength; j++) {
        int tmp = static_cast<int>(A.getDimensionLength(j));
        MPI_Pack(&tmp, 1, MPI_INT, buffer, bufsize, packpos, comm);
    }
    switch (dataClass) {
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        auto* dp = (ArrayOf*)A.getDataPointer();
        for (int i = 0; i < A.getElementCount(); i++) {
            packMPI(dp[i], buffer, bufsize, packpos, comm);
        }
    } break;
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY: {
        stringVector fieldnames(A.getFieldNames());
        int fieldcnt(static_cast<int>(fieldnames.size()));
        MPI_Pack(&fieldcnt, 1, MPI_INT, buffer, bufsize, packpos, comm);
        for (int i = 0; i < fieldcnt; i++) {
            int flen = static_cast<int>(fieldnames[i].size());
            MPI_Pack(&flen, 1, MPI_INT, buffer, bufsize, packpos, comm);
            MPI_Pack(static_cast<const void*>(fieldnames[i].c_str()), flen, MPI_CHAR, buffer,
                bufsize, packpos, comm);
        }
        int isclassType(static_cast<int>(A.isClassType()));
        MPI_Pack(&isclassType, 1, MPI_INT, buffer, bufsize, packpos, comm);
        if (A.isClassType()) {
            ArrayOf classnameAsArray = ArrayOf::characterArrayConstructor(A.getClassType());
            packMPI(classnameAsArray, buffer, bufsize, packpos, comm);
        }
        auto* dp = (ArrayOf*)A.getDataPointer();
        for (int i = 0; i < A.getElementCount() * fieldcnt; i++) {
            packMPI(dp[i], buffer, bufsize, packpos, comm);
        }
    } break;
    case NLS_FUNCTION_HANDLE: {
        function_handle fh = A.getContentAsFunctionHandle();
        AnonymousMacroFunctionDef* cp
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);

        std::string anonymousContent = cp->getContent();
        ArrayOf anonymousElement = ArrayOf::characterArrayConstructor(anonymousContent);
        packMPI(anonymousElement, buffer, bufsize, packpos, comm);

        int isFunctionHandle = (int)(cp->isFunctionHandle());
        MPI_Pack(&isFunctionHandle, 1, MPI_INT, buffer, bufsize, packpos, comm);

        if (!isFunctionHandle) {
            stringVector arguments = cp->getArguments();
            stringVector names = cp->getVariableNames();
            std::vector<ArrayOf> variables = cp->getVariables();
            Dimensions dimsNames(1, names.size());
            ArrayOf fieldnames = ArrayOf::stringArrayConstructor(names, dimsNames);
            Dimensions dimsArguments(1, arguments.size());
            ArrayOf argumentsArrayOf = ArrayOf::stringArrayConstructor(arguments, dimsArguments);
            Dimensions dimsVariables(1, variables.size());
            ArrayOf* cell = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
                NLS_CELL_ARRAY, dimsVariables.getElementCount(), stringVector(), false));
            ArrayOf cellArrayOf = ArrayOf(NLS_CELL_ARRAY, dimsVariables, cell);
            for (size_t k = 0; k < variables.size(); k++) {
                cell[k] = variables[k];
            }

            packMPI(argumentsArrayOf, buffer, bufsize, packpos, comm);
            packMPI(fieldnames, buffer, bufsize, packpos, comm);
            packMPI(cellArrayOf, buffer, bufsize, packpos, comm);
        }
    } break;
    case NLS_LOGICAL:
        if (A.isSparse()) {
            ArrayOf I, J, V, M, N, NNZ;
            bool needToOverload;
            SparseToIJV(A, I, J, V, M, N, NNZ, needToOverload);
            if (needToOverload) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED);
            }
            packMPI(I, buffer, bufsize, packpos, comm);
            packMPI(J, buffer, bufsize, packpos, comm);
            packMPI(V, buffer, bufsize, packpos, comm);
            packMPI(M, buffer, bufsize, packpos, comm);
            packMPI(N, buffer, bufsize, packpos, comm);
            packMPI(NNZ, buffer, bufsize, packpos, comm);
        } else {
            MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(),
                MPI_UINT8_T, buffer, bufsize, packpos, comm);
        }
        break;
    case NLS_UINT8:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(),
            MPI_UINT8_T, buffer, bufsize, packpos, comm);
        break;
    case NLS_INT8:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(), MPI_INT8_T,
            buffer, bufsize, packpos, comm);
        break;
    case NLS_UINT16:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(),
            MPI_UNSIGNED_SHORT, buffer, bufsize, packpos, comm);
        break;
    case NLS_INT16:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(), MPI_SHORT,
            buffer, bufsize, packpos, comm);
        break;
    case NLS_UINT32:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(),
            MPI_UINT32_T, buffer, bufsize, packpos, comm);
        break;
    case NLS_INT32:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(),
            MPI_INT32_T, buffer, bufsize, packpos, comm);
        break;
    case NLS_UINT64:
        MPI_Pack(static_cast<const void*>((void*)A.getDataPointer()), (int)A.getElementCount(),
            MPI_UINT64_T, buffer, bufsize, packpos, comm);
        break;
    case NLS_INT64:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(),
            MPI_INT64_T, buffer, bufsize, packpos, comm);
        break;
    case NLS_SINGLE:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(), MPI_FLOAT,
            buffer, bufsize, packpos, comm);
        break;
    case NLS_MISSING_ARRAY: {
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(), MPI_DOUBLE,
            buffer, bufsize, packpos, comm);
    } break;
    case NLS_DOUBLE:
        if (A.isSparse()) {
            ArrayOf I, J, V, M, N, NNZ;
            bool needToOverload;
            SparseToIJV(A, I, J, V, M, N, NNZ, needToOverload);
            if (needToOverload) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED);
            }
            packMPI(I, buffer, bufsize, packpos, comm);
            packMPI(J, buffer, bufsize, packpos, comm);
            packMPI(V, buffer, bufsize, packpos, comm);
            packMPI(M, buffer, bufsize, packpos, comm);
            packMPI(N, buffer, bufsize, packpos, comm);
            packMPI(NNZ, buffer, bufsize, packpos, comm);
        } else {
            MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(),
                MPI_DOUBLE, buffer, bufsize, packpos, comm);
        }
        break;
    case NLS_SCOMPLEX:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount() * 2,
            MPI_FLOAT, buffer, bufsize, packpos, comm);
        break;
    case NLS_DCOMPLEX:
        if (A.isSparse()) {
            ArrayOf I, J, V, M, N, NNZ;
            bool needToOverload;
            SparseToIJV(A, I, J, V, M, N, NNZ, needToOverload);
            if (needToOverload) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED);
            }
            packMPI(I, buffer, bufsize, packpos, comm);
            packMPI(J, buffer, bufsize, packpos, comm);
            packMPI(V, buffer, bufsize, packpos, comm);
            packMPI(M, buffer, bufsize, packpos, comm);
            packMPI(N, buffer, bufsize, packpos, comm);
            packMPI(NNZ, buffer, bufsize, packpos, comm);
        } else {
            MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount() * 2,
                MPI_DOUBLE, buffer, bufsize, packpos, comm);
        }
        break;
    case NLS_CHAR:
        MPI_Pack(static_cast<const void*>(A.getDataPointer()), (int)A.getElementCount(), MPI_WCHAR,
            buffer, bufsize, packpos, comm);
        break;
    default: {
        Error(_("Type not managed."));
    } break;
    }
}
//=============================================================================
ArrayOf
unpackMPI(void* buffer, int bufsize, int* packpos, MPI_Comm comm)
{
    int idclass;
    MPI_Unpack(buffer, bufsize, packpos, &idclass, 1, MPI_INT, comm);
    NelsonType dataClass = IntToClass(idclass);
    int issparse = 0;
    MPI_Unpack(buffer, bufsize, packpos, &issparse, 1, MPI_INT, comm);
    int dimlength = 0;
    MPI_Unpack(buffer, bufsize, packpos, &dimlength, 1, MPI_INT, comm);
    Dimensions outDim;
    for (int j = 0; j < dimlength; j++) {
        int tmp = 0;
        MPI_Unpack(buffer, bufsize, packpos, &tmp, 1, MPI_INT, comm);
        outDim[j] = tmp;
    }

    void* cp = nullptr;
    switch (dataClass) {
    case NLS_STRING_ARRAY: {
        auto* dp = new ArrayOf[outDim.getElementCount()];
        indexType elementCount = outDim.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            dp[i] = unpackMPI(buffer, bufsize, packpos, comm);
        }
        return ArrayOf(NLS_STRING_ARRAY, outDim, dp);
    } break;
    case NLS_CELL_ARRAY: {
        auto* dp = new ArrayOf[outDim.getElementCount()];
        indexType elementCount = outDim.getElementCount();
        for (indexType i = 0; i < elementCount; i++) {
            dp[i] = unpackMPI(buffer, bufsize, packpos, comm);
        }
        return ArrayOf(NLS_CELL_ARRAY, outDim, dp);
    } break;
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY: {
        int fieldcnt = 0;
        MPI_Unpack(buffer, bufsize, packpos, &fieldcnt, 1, MPI_INT, comm);
        stringVector fieldnames;
        for (int j = 0; j < fieldcnt; j++) {
            int fieldnamelength;
            MPI_Unpack(buffer, bufsize, packpos, &fieldnamelength, 1, MPI_INT, comm);
            char* dbuff = new char[fieldnamelength + 1];
            MPI_Unpack(buffer, bufsize, packpos, dbuff, fieldnamelength, MPI_CHAR, comm);
            dbuff[fieldnamelength] = 0;
            fieldnames.push_back(std::string(dbuff));
            delete[] dbuff;
        }
        int isclassType = 0;
        MPI_Unpack(buffer, bufsize, packpos, &isclassType, 1, MPI_INT, comm);
        std::string classname;
        if (isclassType) {
            ArrayOf classNameAsArray = unpackMPI(buffer, bufsize, packpos, comm);
            classname = classNameAsArray.getContentAsCString();
        }
        indexType elementCount = (indexType)(fieldcnt * outDim.getElementCount());
        auto* dp = new ArrayOf[elementCount];
        for (indexType i = 0; i < elementCount; i++) {
            dp[i] = unpackMPI(buffer, bufsize, packpos, comm);
        }
        ArrayOf returnedArray;
        if (!classname.empty()) {
            returnedArray = ArrayOf(NLS_CLASS_ARRAY, outDim, dp, false, fieldnames);
            returnedArray.setClassType(classname);
        } else {
            returnedArray = ArrayOf(NLS_STRUCT_ARRAY, outDim, dp, false, fieldnames);
        }
        return returnedArray;
    } break;
    case NLS_FUNCTION_HANDLE: {
        function_handle fh;
        fh.anonymousHandle = nullptr;
        ArrayOf anonymousElement = unpackMPI(buffer, bufsize, packpos, comm);
        std::string anonymousContent = anonymousElement.getContentAsCString();

        int isFunctionHandle = 0;
        MPI_Unpack(buffer, bufsize, packpos, &isFunctionHandle, 1, MPI_INT, comm);
        if (isFunctionHandle) {
            fh.anonymousHandle
                = reinterpret_cast<nelson_handle*>(new AnonymousMacroFunctionDef(anonymousContent));
        } else {
            ArrayOf argumentsArrayOf = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf names = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf cellArrayOf = unpackMPI(buffer, bufsize, packpos, comm);

            stringVector fieldnames = names.getContentAsCStringVector(false);
            stringVector arguments = argumentsArrayOf.getContentAsCStringVector(false);
            std::vector<ArrayOf> variables;
            variables.reserve(cellArrayOf.getElementCount());
            ArrayOf* cell = static_cast<ArrayOf*>(
                const_cast<void*>(static_cast<const void*>(cellArrayOf.getDataPointer())));
            for (indexType k = 0; k < cellArrayOf.getElementCount(); k++) {
                variables.push_back(cell[k]);
            }
            fh.anonymousHandle = reinterpret_cast<nelson_handle*>(
                new AnonymousMacroFunctionDef(anonymousContent, arguments, fieldnames, variables));
        }
        if (fh.anonymousHandle == nullptr) {
            Error(_W("A valid function name expected."));
        }
        return ArrayOf::functionHandleConstructor(fh);
    } break;
    case NLS_LOGICAL:
        if (issparse) {
            ArrayOf I = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf J = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf V = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf M = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf N = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf NNZ = unpackMPI(buffer, bufsize, packpos, comm);
            return SparseConstructor(I, J, V, M.getContentAsScalarIndex(),
                N.getContentAsScalarIndex(), NNZ.getContentAsScalarIndex());
        } else {
            cp = ArrayOf::allocateArrayOf(
                NLS_LOGICAL, outDim.getElementCount(), stringVector(), false);
            MPI_Unpack(
                buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT8_T, comm);
        }
        break;
    case NLS_UINT8:
        cp = ArrayOf::allocateArrayOf(NLS_UINT8, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT8_T, comm);
        break;
    case NLS_INT8:
        cp = ArrayOf::allocateArrayOf(NLS_INT8, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_INT8_T, comm);
        break;
    case NLS_UINT16:
        cp = ArrayOf::allocateArrayOf(NLS_UINT16, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(
            buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UNSIGNED_SHORT, comm);
        break;
    case NLS_INT16:
        cp = ArrayOf::allocateArrayOf(NLS_INT16, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_SHORT, comm);
        break;
    case NLS_UINT32:
        cp = ArrayOf::allocateArrayOf(NLS_UINT32, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT32_T, comm);
        break;
    case NLS_INT32:
        cp = ArrayOf::allocateArrayOf(NLS_INT32, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_INT32_T, comm);
        break;
    case NLS_UINT64:
        cp = ArrayOf::allocateArrayOf(NLS_UINT64, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT64_T, comm);
        break;
    case NLS_INT64:
        cp = ArrayOf::allocateArrayOf(NLS_INT64, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_INT64_T, comm);
        break;
    case NLS_SINGLE:
        cp = ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_FLOAT, comm);
        break;
    case NLS_MISSING_ARRAY: {
        cp = ArrayOf::allocateArrayOf(
            NLS_MISSING_ARRAY, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_DOUBLE, comm);
    } break;
    case NLS_DOUBLE:
        if (issparse) {
            ArrayOf I = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf J = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf V = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf M = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf N = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf NNZ = unpackMPI(buffer, bufsize, packpos, comm);
            return SparseConstructor(I, J, V, M.getContentAsScalarIndex(),
                N.getContentAsScalarIndex(), NNZ.getContentAsScalarIndex());
        } else {
            cp = ArrayOf::allocateArrayOf(
                NLS_DOUBLE, outDim.getElementCount(), stringVector(), false);
            MPI_Unpack(
                buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_DOUBLE, comm);
        }
        break;
    case NLS_SCOMPLEX:
        cp = ArrayOf::allocateArrayOf(
            NLS_SCOMPLEX, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(
            buffer, bufsize, packpos, cp, (int)outDim.getElementCount() * 2, MPI_FLOAT, comm);
        break;
    case NLS_DCOMPLEX:
        if (issparse) {
            ArrayOf I = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf J = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf V = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf M = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf N = unpackMPI(buffer, bufsize, packpos, comm);
            ArrayOf NNZ = unpackMPI(buffer, bufsize, packpos, comm);
            return SparseConstructor(I, J, V, M.getContentAsScalarIndex(),
                N.getContentAsScalarIndex(), NNZ.getContentAsScalarIndex());
        } else {
            cp = ArrayOf::allocateArrayOf(
                NLS_DCOMPLEX, outDim.getElementCount(), stringVector(), true);
            MPI_Unpack(
                buffer, bufsize, packpos, cp, (int)outDim.getElementCount() * 2, MPI_DOUBLE, comm);
        }
        break;
    case NLS_CHAR:
        cp = ArrayOf::allocateArrayOf(NLS_CHAR, outDim.getElementCount(), stringVector(), false);
        MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_WCHAR, comm);
        break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return ArrayOf(dataClass, outDim, cp);
}
//=============================================================================
int
getCanonicalSize(int count, MPI_Datatype atype, MPI_Comm comm)
{
    int size = 0;
    MPI_Pack_size(count, atype, comm, &size);
    return size;
}
//=============================================================================
int
getArrayOfFootPrint(ArrayOf& A, MPI_Comm comm)
{
    unsigned int overhead = getCanonicalSize(maxDims + 1, MPI_INT, comm);
    NelsonType dataClass(A.getDataClass());
    switch (dataClass) {
    case NLS_LOGICAL:
        if (A.isSparse()) {
            ArrayOf I, J, V, M, N, NNZ;
            bool needToOverload;
            SparseToIJV(A, I, J, V, M, N, NNZ, needToOverload);
            if (needToOverload) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED);
            }
            int sI = getArrayOfFootPrint(I, comm);
            int sJ = getArrayOfFootPrint(J, comm);
            int sV = getArrayOfFootPrint(V, comm);
            int sM = getArrayOfFootPrint(M, comm);
            int sN = getArrayOfFootPrint(N, comm);
            int sNNZ = getArrayOfFootPrint(NNZ, comm);
            return (overhead + sI + sJ + sV + sM + sN + sNNZ);
        } else {
            return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_UINT8_T, comm));
        }
    case NLS_UINT8:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_UINT8_T, comm));
    case NLS_INT8:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_INT8_T, comm));
    case NLS_UINT16:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_UNSIGNED_SHORT, comm));
    case NLS_INT16:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_SHORT, comm));
    case NLS_UINT32:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_UINT32_T, comm));
    case NLS_INT32:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_INT32_T, comm));
    case NLS_UINT64:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_UINT64_T, comm));
    case NLS_INT64:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_INT64_T, comm));
    case NLS_SINGLE:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_FLOAT, comm));
    case NLS_DOUBLE:
        if (A.isSparse()) {
            ArrayOf I, J, V, M, N, NNZ;
            bool needToOverload;
            SparseToIJV(A, I, J, V, M, N, NNZ, needToOverload);
            if (needToOverload) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED);
            }
            int sI = getArrayOfFootPrint(I, comm);
            int sJ = getArrayOfFootPrint(J, comm);
            int sV = getArrayOfFootPrint(V, comm);
            int sM = getArrayOfFootPrint(M, comm);
            int sN = getArrayOfFootPrint(N, comm);
            int sNNZ = getArrayOfFootPrint(NNZ, comm);
            return (overhead + sI + sJ + sV + sM + sN + sNNZ);
        } else {
            return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_DOUBLE, comm));
        }
    case NLS_SCOMPLEX:
        return (overhead + getCanonicalSize((int)A.getElementCount() * 2, MPI_FLOAT, comm));
    case NLS_DCOMPLEX:
        if (A.isSparse()) {
            ArrayOf I, J, V, M, N, NNZ;
            bool needToOverload;
            SparseToIJV(A, I, J, V, M, N, NNZ, needToOverload);
            if (needToOverload) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED);
            }
            int sI = getArrayOfFootPrint(I, comm);
            int sJ = getArrayOfFootPrint(J, comm);
            int sV = getArrayOfFootPrint(V, comm);
            int sM = getArrayOfFootPrint(M, comm);
            int sN = getArrayOfFootPrint(N, comm);
            int sNNZ = getArrayOfFootPrint(NNZ, comm);
            return (overhead + sI + sJ + sV + sM + sN + sNNZ);
        } else {
            return (overhead + getCanonicalSize((int)A.getElementCount() * 2, MPI_DOUBLE, comm));
        }
    case NLS_CHAR:
        return (overhead + getCanonicalSize((int)A.getElementCount(), MPI_WCHAR, comm));
    case NLS_STRUCT_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_CLASS_ARRAY: {
        if (dataClass == NLS_CELL_ARRAY || dataClass == NLS_STRING_ARRAY) {
            int total = 0;
            auto* dp = (ArrayOf*)A.getDataPointer();
            for (int i = 0; i < A.getElementCount(); i++) {
                total += getArrayOfFootPrint(dp[i], comm);
            }
            return (total + overhead);
        }
        stringVector fieldnames(A.getFieldNames());
        int fieldcount = static_cast<int>(fieldnames.size());
        int fieldsize = getCanonicalSize(1, MPI_INT, comm);
        for (int j = 0; j < fieldcount; j++) {
            fieldsize += getCanonicalSize(1, MPI_INT, comm)
                + getCanonicalSize((int)fieldnames[j].size(), MPI_CHAR, comm);
        }
        fieldsize += getCanonicalSize(1, MPI_INT, comm);
        int isclassType(static_cast<int>(A.isClassType()));
        if (isclassType) {
            ArrayOf classnameAsArray = ArrayOf::characterArrayConstructor(A.getClassType());
            fieldsize += getCanonicalSize((int)classnameAsArray.getElementCount(), MPI_WCHAR, comm);
        }
        auto* dp = (ArrayOf*)A.getDataPointer();
        int total = 0;
        for (int i = 0; i < A.getElementCount() * fieldcount; i++) {
            total += getArrayOfFootPrint(dp[i], comm);
        }
        return (total + overhead + fieldsize + 1);
    } break;
    case NLS_FUNCTION_HANDLE: {
        int total = overhead;
        function_handle fh = A.getContentAsFunctionHandle();
        AnonymousMacroFunctionDef* cp
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        std::string anonymousContent = cp->getContent();
        ArrayOf anonymousElement = ArrayOf::characterArrayConstructor(anonymousContent);
        total += getArrayOfFootPrint(anonymousElement, comm);

        int isFunctionHandle = (int)(cp->isFunctionHandle());
        total += 1;
        if (!isFunctionHandle) {
            stringVector arguments = cp->getArguments();
            stringVector names = cp->getVariableNames();
            std::vector<ArrayOf> variables = cp->getVariables();
            Dimensions dimsNames(1, names.size());
            ArrayOf fieldnames = ArrayOf::stringArrayConstructor(names, dimsNames);
            Dimensions dimsArguments(1, arguments.size());
            ArrayOf argumentsArrayOf = ArrayOf::stringArrayConstructor(arguments, dimsArguments);
            Dimensions dimsVariables(1, variables.size());
            ArrayOf* cell = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
                NLS_CELL_ARRAY, dimsVariables.getElementCount(), stringVector(), false));
            ArrayOf cellArrayOf = ArrayOf(NLS_CELL_ARRAY, dimsVariables, cell);
            for (size_t k = 0; k < variables.size(); k++) {
                cell[k] = variables[k];
            }
            total += getArrayOfFootPrint(argumentsArrayOf, comm);
            total += getArrayOfFootPrint(fieldnames, comm);
            total += getArrayOfFootPrint(cellArrayOf, comm);
        }
        return total;

    } break;
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN:
    default: {
    } break;
    }
    return 0;
}
//=============================================================================
std::string
getMpiLibraryVersion()
{
    char library_version[MPI_MAX_LIBRARY_VERSION_STRING];
#ifdef OMPI_MAJOR_VERSION
    sprintf(library_version, "OpenMPI %d.%d.%d", OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION);
    std::string returnedString = library_version;
#else
#if MPI_VERSION > 1
    int resultlen = 0;
    MPI_Get_library_version(library_version, &resultlen);
    if (resultlen >= MPI_MAX_LIBRARY_VERSION_STRING) {
        resultlen = MPI_MAX_LIBRARY_VERSION_STRING - 1;
    }
    library_version[resultlen] = 0;
    std::string returnedString = library_version;
#else
    std::string returnedString = "Unknown MPI version < 2";
#endif
#endif
    return returnedString;
}
//=============================================================================
MPI_Op
stringToMpiOp(const std::wstring& op_str)
{
    MPI_Op mpi_op = MPI_OP_NULL;
    if (op_str == L"MPI_MAX") {
        return MPI_MAX;
    }
    if (op_str == L"MPI_MIN") {
        return MPI_MIN;
    }
    if (op_str == L"MPI_SUM") {
        return MPI_SUM;
    }
    if (op_str == L"MPI_PROD") {
        return MPI_PROD;
    }
    if (op_str == L"MPI_LAND") {
        return MPI_LAND;
    }
    if (op_str == L"MPI_LOR") {
        return MPI_LOR;
    }
    if (op_str == L"MPI_BAND") {
        return MPI_BAND;
    }
    if (op_str == L"MPI_BOR") {
        return MPI_BOR;
    }
    if (op_str == L"MPI_LXOR") {
        return MPI_LXOR;
    }
    if (op_str == L"MPI_BXOR") {
        return MPI_BXOR;
    }
    return mpi_op;
}
//=============================================================================
std::string
getMpiCommName(MPI_Comm comm)
{
    char name[MPI_MAX_OBJECT_NAME];
    name[0] = 0;
    int len;
    if (MPI_Comm_get_name(comm, name, &len) != MPI_SUCCESS) {
        Error(_W("Invalid communicator"));
    }
    if (len >= MPI_MAX_OBJECT_NAME) {
        len = MPI_MAX_OBJECT_NAME - 1;
    }
    name[len] = 0;
    return std::string(name);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
