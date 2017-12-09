//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <mpi.h>
#include <boost/container/vector.hpp>
#include "SparseConstructors.hpp"
#include "SparseToIJV.hpp"
#include "MPI_helpers.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
#ifndef MPI_MAX_LIBRARY_VERSION_STRING
#define MPI_MAX_LIBRARY_VERSION_STRING 64
#endif
    //=============================================================================
    static MPI_Errhandler errhdl;
    //=============================================================================
    void MPIErrorHandler(MPI_Comm *comm, int *errorcode, ...)
    {
        char buffer[MPI_MAX_ERROR_STRING];
        int resultlen = 0;
        MPI_Error_string(*errorcode, buffer, &resultlen);
        buffer[resultlen] = 0;
        throw Exception(buffer);
    }
    //=============================================================================
    int initializeMPI()
    {
        int flag = 1;
        MPI_Initialized(&flag);
        if (flag)
        {
            return flag;
        }
        MPI_Init(NULL, NULL);
        MPI_Comm_create_errhandler(MPIErrorHandler, &errhdl);
        MPI_Comm_set_errhandler(MPI_COMM_WORLD, errhdl);
        MPI_Initialized(&flag);
        return flag;
    }
    //=============================================================================
    static Class IntToClass(int code)
    {
        return (Class)(code / 1000);
    }
    //=============================================================================
    static int ClassToInt(Class dataClass)
    {
        return (int)(dataClass * 1000);
    }
    //=============================================================================
    void packMPI(ArrayOf &A, void *buffer, int bufsize, int *packpos, MPI_Comm comm)
    {
        Class dataClass(A.getDataClass());
        int idclass = ClassToInt(dataClass);
        MPI_Pack(&idclass, 1, MPI_INT, buffer, bufsize, packpos, comm);
        int issparse = (int)A.isSparse();
        MPI_Pack(&issparse, 1, MPI_INT, buffer, bufsize, packpos, comm);
        int dimlength = (int)A.getDimensions().getLength();
        MPI_Pack(&dimlength, 1, MPI_INT, buffer, bufsize, packpos, comm);
        for (int j = 0; j < dimlength; j++)
        {
            int tmp = (int)A.getDimensionLength(j);
            MPI_Pack(&tmp, 1, MPI_INT, buffer, bufsize, packpos, comm);
        }
        if (A.isReferenceType())
        {
            if (dataClass == NLS_CELL_ARRAY)
            {
                ArrayOf *dp = (ArrayOf *)A.getDataPointer();
                for (int i = 0; i < A.getLength(); i++)
                {
                    packMPI(dp[i], buffer, bufsize, packpos, comm);
                }
            }
            else
            {
                stringVector fieldnames(A.getFieldNames());
                int fieldcnt((int)fieldnames.size());
                MPI_Pack(&fieldcnt, 1, MPI_INT, buffer, bufsize, packpos, comm);
                for (int i = 0; i < fieldcnt; i++)
                {
                    int flen = (int)fieldnames[i].size();
                    MPI_Pack(&flen, 1, MPI_INT, buffer, bufsize, packpos, comm);
                    MPI_Pack((void*)fieldnames[i].c_str(), flen, MPI_CHAR, buffer, bufsize, packpos, comm);
                }
                ArrayOf *dp = (ArrayOf *)A.getDataPointer();
                for (int i = 0; i < A.getLength()*fieldcnt; i++)
                {
                    packMPI(dp[i], buffer, bufsize, packpos, comm);
                }
            }
        }
        else
        {
            switch (dataClass)
            {
                case NLS_LOGICAL:
                    if (A.isSparse())
                    {
                        ArrayOf I, J, V, M, N, NNZ;
                        SparseToIJV(A, I, J, V, M, N, NNZ);
                        packMPI(I, buffer, bufsize, packpos, comm);
                        packMPI(J, buffer, bufsize, packpos, comm);
                        packMPI(V, buffer, bufsize, packpos, comm);
                        packMPI(M, buffer, bufsize, packpos, comm);
                        packMPI(N, buffer, bufsize, packpos, comm);
                        packMPI(NNZ, buffer, bufsize, packpos, comm);
                    }
                    else
                    {
                        MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_UINT8_T, buffer, bufsize, packpos, comm);
                    }
                    break;
                case NLS_UINT8:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_UINT8_T, buffer, bufsize, packpos, comm);
                    break;
                case NLS_INT8:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_INT8_T, buffer, bufsize, packpos, comm);
                    break;
                case NLS_UINT16:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_UINT16_T, buffer, bufsize, packpos, comm);
                    break;
                case NLS_INT16:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_INT16_T, buffer, bufsize, packpos, comm);
                    break;
                case NLS_UINT32:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_UINT32_T, buffer, bufsize, packpos, comm);
                    break;
                case NLS_INT32:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_INT32_T, buffer, bufsize, packpos, comm);
                    break;
                case NLS_UINT64:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_UINT64_T, buffer, bufsize, packpos, comm);
                    break;
                case NLS_INT64:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_INT64_T, buffer, bufsize, packpos, comm);
                    break;
                case NLS_SINGLE:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_FLOAT, buffer, bufsize, packpos, comm);
                    break;
                case NLS_DOUBLE:
                    if (A.isSparse())
                    {
                        ArrayOf I, J, V, M, N, NNZ;
                        SparseToIJV(A, I, J, V, M, N, NNZ);
                        packMPI(I, buffer, bufsize, packpos, comm);
                        packMPI(J, buffer, bufsize, packpos, comm);
                        packMPI(V, buffer, bufsize, packpos, comm);
                        packMPI(M, buffer, bufsize, packpos, comm);
                        packMPI(N, buffer, bufsize, packpos, comm);
                        packMPI(NNZ, buffer, bufsize, packpos, comm);
                    }
                    else
                    {
                        MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_DOUBLE, buffer, bufsize, packpos, comm);
                    }
                    break;
                case NLS_SCOMPLEX:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength() * 2, MPI_FLOAT, buffer, bufsize, packpos, comm);
                    break;
                case NLS_DCOMPLEX:
                    if (A.isSparse())
                    {
                        ArrayOf I, J, V, M, N, NNZ;
                        SparseToIJV(A, I, J, V, M, N, NNZ);
                        packMPI(I, buffer, bufsize, packpos, comm);
                        packMPI(J, buffer, bufsize, packpos, comm);
                        packMPI(V, buffer, bufsize, packpos, comm);
                        packMPI(M, buffer, bufsize, packpos, comm);
                        packMPI(N, buffer, bufsize, packpos, comm);
                        packMPI(NNZ, buffer, bufsize, packpos, comm);
                    }
                    else
                    {
                        MPI_Pack((void *)A.getDataPointer(), (int)A.getLength() * 2 , MPI_DOUBLE, buffer, bufsize, packpos, comm);
                    }
                    break;
                case NLS_CHAR:
                    MPI_Pack((void *)A.getDataPointer(), (int)A.getLength(), MPI_WCHAR, buffer, bufsize, packpos, comm);
                    break;
                default:
                {
                    throw Exception(_("Type not managed."));
                }
                break;
            }
        }
    }
    //=============================================================================
    ArrayOf unpackMPI(void *buffer, int bufsize, int *packpos, MPI_Comm comm)
    {
        int idclass;
        MPI_Unpack(buffer, bufsize, packpos, &idclass, 1, MPI_INT, comm);
        Class dataClass = IntToClass(idclass);
        int issparse = 0;
        MPI_Unpack(buffer, bufsize, packpos, &issparse, 1, MPI_INT, comm);
        int dimlength = 0;
        MPI_Unpack(buffer, bufsize, packpos, &dimlength, 1, MPI_INT, comm);
        Dimensions outDim;
        for (int j = 0; j<dimlength; j++)
        {
            int tmp = 0;
            MPI_Unpack(buffer, bufsize, packpos, &tmp, 1, MPI_INT, comm);
            outDim[j] = tmp;
        }
        if (dataClass == NLS_CELL_ARRAY)
        {
            ArrayOf *dp = new ArrayOf[outDim.getElementCount()];
            for (int i = 0; i < outDim.getElementCount(); i++)
            {
                dp[i] = unpackMPI(buffer, bufsize, packpos, comm);
            }
            return ArrayOf(NLS_CELL_ARRAY, outDim, dp);
        }
        else if (dataClass == NLS_STRUCT_ARRAY)
        {
            int fieldcnt = 0;
            MPI_Unpack(buffer, bufsize, packpos, &fieldcnt, 1, MPI_INT, comm);
            stringVector fieldnames;
            for (int j = 0; j<fieldcnt; j++)
            {
                int fieldnamelength;
                MPI_Unpack(buffer, bufsize, packpos, &fieldnamelength, 1, MPI_INT, comm);
                char *dbuff = new char[fieldnamelength + 1];
                MPI_Unpack(buffer, bufsize, packpos, dbuff, fieldnamelength, MPI_CHAR, comm);
                dbuff[fieldnamelength] = 0;
                fieldnames.push_back(std::string(dbuff));
                delete [] dbuff;
            }
            ArrayOf *dp = new ArrayOf[fieldcnt*outDim.getElementCount()];
            for (int i = 0; i < fieldcnt*outDim.getElementCount(); i++)
            {
                dp[i] = unpackMPI(buffer, bufsize, packpos, comm);
            }
            return ArrayOf(NLS_STRUCT_ARRAY, outDim, dp, false, fieldnames);
        }
        void *cp = nullptr;
        switch (dataClass)
        {
            case NLS_LOGICAL:
                if (issparse)
                {
                    ArrayOf I = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf J = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf V = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf M = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf N = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf NNZ = unpackMPI(buffer, bufsize, packpos, comm);
                    return SparseConstructor(I, J, V, M.getContentAsScalarIndex(), N.getContentAsScalarIndex(), NNZ.getContentAsScalarIndex());
                }
                else
                {
                    cp = ArrayOf::allocateArrayOf(NLS_LOGICAL, outDim.getElementCount());
                    MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT8_T, comm);
                }
                break;
            case NLS_UINT8:
                cp = ArrayOf::allocateArrayOf(NLS_UINT8, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT8_T, comm);
                break;
            case NLS_INT8:
                cp = ArrayOf::allocateArrayOf(NLS_INT8, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_INT8_T, comm);
                break;
            case NLS_UINT16:
                cp = ArrayOf::allocateArrayOf(NLS_UINT16, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT8_T, comm);
                break;
            case NLS_INT16:
                cp = ArrayOf::allocateArrayOf(NLS_INT16, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_INT8_T, comm);
                break;
            case NLS_UINT32:
                cp = ArrayOf::allocateArrayOf(NLS_UINT32, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT32_T, comm);
                break;
            case NLS_INT32:
                cp = ArrayOf::allocateArrayOf(NLS_INT32, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_INT32_T, comm);
                break;
            case NLS_UINT64:
                cp = ArrayOf::allocateArrayOf(NLS_UINT64, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_UINT64_T, comm);
                break;
            case NLS_INT64:
                cp = ArrayOf::allocateArrayOf(NLS_INT64, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_INT64_T, comm);
                break;
            case NLS_SINGLE:
                cp = ArrayOf::allocateArrayOf(NLS_SINGLE, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_FLOAT, comm);
                break;
            case NLS_DOUBLE:
                if (issparse)
                {
                    ArrayOf I = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf J = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf V = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf M = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf N = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf NNZ = unpackMPI(buffer, bufsize, packpos, comm);
                    return SparseConstructor(I, J, V, M.getContentAsScalarIndex(), N.getContentAsScalarIndex(), NNZ.getContentAsScalarIndex());
                }
                else
                {
                    cp = ArrayOf::allocateArrayOf(NLS_DOUBLE, outDim.getElementCount());
                    MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_DOUBLE, comm);
                }
                break;
            case NLS_SCOMPLEX:
                cp = ArrayOf::allocateArrayOf(NLS_SCOMPLEX, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount() * 2, MPI_FLOAT, comm);
                break;
            case NLS_DCOMPLEX:
                if (issparse)
                {
                    ArrayOf I = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf J = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf V = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf M = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf N = unpackMPI(buffer, bufsize, packpos, comm);
                    ArrayOf NNZ = unpackMPI(buffer, bufsize, packpos, comm);
                    return SparseConstructor(I, J, V, M.getContentAsScalarIndex(), N.getContentAsScalarIndex(), NNZ.getContentAsScalarIndex());
                }
                else
                {
                    cp = ArrayOf::allocateArrayOf(NLS_DCOMPLEX, outDim.getElementCount());
                    MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount() * 2, MPI_DOUBLE, comm);
                }
                break;
            case NLS_CHAR:
                cp = ArrayOf::allocateArrayOf(NLS_CHAR, outDim.getElementCount());
                MPI_Unpack(buffer, bufsize, packpos, cp, (int)outDim.getElementCount(), MPI_WCHAR, comm);
                break;
            default:
            {
                throw Exception(_W("Type not managed."));
            }
            break;
        }
        return ArrayOf(dataClass, outDim, cp);
    }
    //=============================================================================
    int getCanonicalSize(int count, MPI_Datatype atype, MPI_Comm comm)
    {
        int size = 0;
        MPI_Pack_size(count, atype, comm, &size);
        return size;
    }
    //=============================================================================
    int getArrayOfFootPrint(ArrayOf &A, MPI_Comm comm)
    {
        unsigned int overhead = getCanonicalSize(maxDims + 1, MPI_INT, comm);
        Class dataClass(A.getDataClass());
        if (A.isReferenceType())
        {
            if (dataClass == NLS_CELL_ARRAY)
            {
                int total = 0;
                ArrayOf *dp = (ArrayOf *)A.getDataPointer();
                for (int i = 0; i < A.getLength(); i++)
                {
                    total += getArrayOfFootPrint(dp[i], comm);
                }
                return (total + overhead);
            }
            else
            {
                stringVector fieldnames(A.getFieldNames());
                int fieldcount = (int)fieldnames.size();
                int fieldsize = getCanonicalSize(1, MPI_INT, comm);
                for (int j = 0; j < fieldcount; j++)
                {
                    fieldsize += getCanonicalSize(1, MPI_INT, comm) +
                                 getCanonicalSize((int)fieldnames[j].size(), MPI_CHAR, comm);
                }
                ArrayOf *dp = (ArrayOf *)A.getDataPointer();
                int total = 0;
                for (int i = 0; i < A.getLength()*fieldcount; i++)
                {
                    total += getArrayOfFootPrint(dp[i], comm);
                }
                return (total + overhead + fieldsize + 1);
            }
        }
        switch (dataClass)
        {
            case NLS_LOGICAL:
                if (A.isSparse())
                {
					ArrayOf I, J, V, M, N, NNZ;
					SparseToIJV(A, I, J, V, M, N, NNZ);
					int sI = getArrayOfFootPrint(I, comm);
					int sJ = getArrayOfFootPrint(J, comm);
					int sV = getArrayOfFootPrint(V, comm);
					int sM = getArrayOfFootPrint(M, comm);
					int sN = getArrayOfFootPrint(N, comm);
					int sNNZ = getArrayOfFootPrint(NNZ, comm);
					return(overhead + sI + sJ + sV + sM + sN + sNNZ);
				}
                else
                {
                    return(overhead + getCanonicalSize((int)A.getLength(), MPI_UINT8_T, comm));
                }
            case NLS_UINT8:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_UINT8_T, comm));
            case NLS_INT8:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_INT8_T, comm));
            case NLS_UINT16:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_UINT16_T, comm));
            case NLS_INT16:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_INT16_T, comm));
            case NLS_UINT32:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_UINT32_T, comm));
            case NLS_INT32:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_INT32_T, comm));
            case NLS_UINT64:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_UINT64_T, comm));
            case NLS_INT64:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_INT64_T, comm));
            case NLS_SINGLE:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_FLOAT, comm));
            case NLS_DOUBLE:
                if (A.isSparse())
                {
                    ArrayOf I, J, V, M, N, NNZ;
                    SparseToIJV(A, I, J, V, M, N, NNZ);
                    int sI = getArrayOfFootPrint(I, comm);
                    int sJ = getArrayOfFootPrint(J, comm);
                    int sV = getArrayOfFootPrint(V, comm);
                    int sM = getArrayOfFootPrint(M, comm);
                    int sN = getArrayOfFootPrint(N, comm);
                    int sNNZ = getArrayOfFootPrint(NNZ, comm);
                    return(overhead + sI + sJ + sV + sM + sN + sNNZ);
                }
                else
                {
                    return(overhead + getCanonicalSize((int)A.getLength(), MPI_DOUBLE, comm));
                }
            case NLS_SCOMPLEX:
                return(overhead + getCanonicalSize((int)A.getLength() * 2, MPI_FLOAT, comm));
            case NLS_DCOMPLEX:
                if (A.isSparse())
                {
					ArrayOf I, J, V, M, N, NNZ;
					SparseToIJV(A, I, J, V, M, N, NNZ);
					int sI = getArrayOfFootPrint(I, comm);
					int sJ = getArrayOfFootPrint(J, comm);
					int sV = getArrayOfFootPrint(V, comm);
					int sM = getArrayOfFootPrint(M, comm);
					int sN = getArrayOfFootPrint(N, comm);
					int sNNZ = getArrayOfFootPrint(NNZ, comm);
					return(overhead + sI + sJ + sV + sM + sN + sNNZ);
				}
                else
                {
                    return(overhead + getCanonicalSize((int)A.getLength() * 2, MPI_DOUBLE, comm));
                }
            case NLS_CHAR:
                return(overhead + getCanonicalSize((int)A.getLength(), MPI_WCHAR, comm));
        }
        return 0;
    }
    //=============================================================================
    std::string getMpiLibraryVersion()
    {
        char library_version[MPI_MAX_LIBRARY_VERSION_STRING];
#ifdef OMPI_MAJOR_VERSION
        sprintf(library_version, "OpenMPI %d.%d.%d", OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION, OMPI_RELEASE_VERSION);
        std::string returnedString = library_version;
#else
#if MPI_VERSION > 1
        int resultlen = 0;
        MPI_Get_library_version(library_version, &resultlen);
        library_version[resultlen] = 0;
        std::string returnedString = library_version;
#else
        std::string returnedString = "Unknown MPI version < 2";
#endif
#endif
        return returnedString;
    }
    //=============================================================================
}
//=============================================================================
