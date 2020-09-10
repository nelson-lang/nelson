//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define EIGEN_NO_DEBUG
//=============================================================================
#include <Eigen/Sparse>
#include <boost/serialization/string.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/interprocess/ipc/message_queue.hpp>
#include <boost/thread/thread.hpp>
#include "NelsonInterprocess.hpp"
#include "NelsonPIDs.hpp"
#include "characters_encoding.hpp"
#include "PostCommand.hpp"
#include "MainEvaluator.hpp"
#include "StringZLib.hpp"
#include "SparseConstructors.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr auto NELSON_COMMAND_INTERPROCESS = "NELSON_COMMAND_INTERPROCESS";
constexpr auto OFF_MSG_SIZE = sizeof(double) * 16 * 1024;
constexpr auto MAX_MSG_SIZE = sizeof(double) * (4096 * 4096) + OFF_MSG_SIZE;
constexpr auto MAX_NB_MSG = 4;
//=============================================================================
static bool receiverLoopRunning = false;
//=============================================================================
static boost::thread* receiver_thread = nullptr;
//=============================================================================
class nelsonObject
{
public:
    //=============================================================================
    nelsonObject() { clear(); }
    //=============================================================================
    nelsonObject(const ArrayOf& data)
    {
        clear();
        set(data);
    }
    //=============================================================================
    void
    clear()
    {
        nelsonObjectClass = (int)NLS_NOT_TYPED;
        isSparse = false;
        dims.clear();
        fieldnames.clear();
        asInt8.clear();
        asUint8.clear();
        asInt16.clear();
        asUint16.clear();
        asInt32.clear();
        asUint32.clear();
        asInt64.clear();
        asUint64.clear();
        asDouble.clear();
        asSingle.clear();
        asCharacter.clear();
        nzmax = 0;
        I.clear();
        J.clear();
        otherObject.clear();
    }
    //=============================================================================
    bool
    set(const ArrayOf& data)
    {
        nelsonObjectClass = (int)data.getDataClass();
        isSparse = data.isSparse();
        Dimensions dimsData = data.getDimensions();
        indexType length = dimsData.getLength();
        dims.reserve(length);
        for (indexType k = 0; k < length; ++k) {
            dims.push_back(dimsData.getAt(k));
        }
        switch (data.getDataClass()) {
        case NLS_CELL_ARRAY: {
            ArrayOf* elements = (ArrayOf*)data.getDataPointer();
            indexType nbElements = dimsData.getElementCount();
            otherObject.reserve(nbElements);
            for (indexType k = 0; k < nbElements; ++k) {
                otherObject.push_back(nelsonObject(elements[k]));
            }
        } break;
        case NLS_STRUCT_ARRAY: {
            ArrayOf* elements = (ArrayOf*)data.getDataPointer();
            indexType nbElements = dimsData.getElementCount();
            fieldnames = data.getFieldNames();
            otherObject.reserve(nbElements);
            for (indexType k = 0; k < nbElements; ++k) {
                otherObject.push_back(nelsonObject(elements[k]));
            }
        } break;
        case NLS_STRING_ARRAY: {
            ArrayOf* elements = (ArrayOf*)data.getDataPointer();
            indexType nbElements = dimsData.getElementCount();
            otherObject.reserve(nbElements);
            for (indexType k = 0; k < nbElements; ++k) {
                otherObject.push_back(nelsonObject(elements[k]));
            }
        } break;
        case NLS_LOGICAL: {
            if (!isSparse) {
                logical* ptrLogical = (logical*)data.getDataPointer();
                asUint8.reserve(dimsData.getElementCount());
                asUint8.assign(ptrLogical, ptrLogical + dimsData.getElementCount());
            } else {
                const Eigen::SparseMatrix<uint8, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<uint8, 0, signedIndexType>*)data.getSparseDataPointer();
                if (spMat) {
                    eigenSparseToIJV<uint8>(*spMat, I, J, asUint8, nzmax);
                }
            }
            return true;
        } break;
        case NLS_UINT8: {
            uint8* ptrUint8 = (uint8*)data.getDataPointer();
            asUint8.reserve(dimsData.getElementCount());
            asUint8.assign(ptrUint8, ptrUint8 + dimsData.getElementCount());
            return true;
        } break;
        case NLS_INT8: {
            int8* ptrInt8 = (int8*)data.getDataPointer();
            asInt8.reserve(dimsData.getElementCount());
            asInt8.assign(ptrInt8, ptrInt8 + dimsData.getElementCount());
            return true;
        } break;
        case NLS_UINT16: {
            uint16* ptrUint16 = (uint16*)data.getDataPointer();
            asUint16.reserve(dimsData.getElementCount());
            asUint16.assign(ptrUint16, ptrUint16 + dimsData.getElementCount());
            return true;
        } break;
        case NLS_INT16: {
            int16* ptrInt16 = (int16*)data.getDataPointer();
            asInt16.reserve(dimsData.getElementCount());
            asInt16.assign(ptrInt16, ptrInt16 + dimsData.getElementCount());
            return true;
        } break;
        case NLS_UINT32: {
            uint32* ptrUint32 = (uint32*)data.getDataPointer();
            asUint32.reserve(dimsData.getElementCount());
            asUint32.assign(ptrUint32, ptrUint32 + dimsData.getElementCount());
            return true;
        } break;
        case NLS_INT32: {
            int32* ptrInt32 = (int32*)data.getDataPointer();
            asInt32.reserve(dimsData.getElementCount());
            asInt32.assign(ptrInt32, ptrInt32 + dimsData.getElementCount());
            return true;
        } break;
        case NLS_UINT64: {
            uint64* ptrUint64 = (uint64*)data.getDataPointer();
            asUint64.reserve(dimsData.getElementCount());
            asUint64.assign(ptrUint64, ptrUint64 + dimsData.getElementCount());
            return true;
        } break;
        case NLS_INT64: {
            int64* ptrInt64 = (int64*)data.getDataPointer();
            asInt64.reserve(dimsData.getElementCount());
            asInt64.assign(ptrInt64, ptrInt64 + dimsData.getElementCount());
            return true;
        } break;
        case NLS_SINGLE: {
            single* ptrSingle = (single*)data.getDataPointer();
            asSingle.reserve(dimsData.getElementCount());
            asSingle.assign(ptrSingle, ptrSingle + dimsData.getElementCount());
            return true;
        } break;
        case NLS_DOUBLE: {
            if (!isSparse) {
                double* ptrDouble = (double*)data.getDataPointer();
                asDouble.reserve(dimsData.getElementCount());
                asDouble.assign(ptrDouble, ptrDouble + dimsData.getElementCount());
            } else {
                const Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<double, 0, signedIndexType>*)data.getSparseDataPointer();
                if (spMat) {
                    eigenSparseToIJV<double>(*spMat, I, J, asDouble, nzmax);
                }
            }
            return true;
        } break;
        case NLS_SCOMPLEX: {
            single* ptrSingle = (single*)data.getDataPointer();
            asSingle.reserve(dimsData.getElementCount() * 2);
            asSingle.assign(ptrSingle, ptrSingle + (dimsData.getElementCount() * 2));
            return true;
        } break;
        case NLS_DCOMPLEX: {
            if (!isSparse) {
                double* ptrDouble = (double*)data.getDataPointer();
                asDouble.reserve(dimsData.getElementCount() * 2);
                asDouble.assign(ptrDouble, ptrDouble + (dimsData.getElementCount() * 2));
            } else {
                const Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)
                          data.getSparseDataPointer();
                if (spMat) {
                    std::vector<doublecomplex> V;
                    eigenSparseToIJV<doublecomplex>(*spMat, I, J, V, nzmax);
                    double* Vz = reinterpret_cast<double*>(V.data());
                    asDouble.reserve(V.size() * 2);
                    asDouble.assign(Vz, Vz + (V.size() * 2));
                }
            }
            return true;
        } break;
        case NLS_CHAR: {
            charType* ptrCharacter = (charType*)data.getDataPointer();
            asCharacter.reserve(dimsData.getElementCount());
            asCharacter.assign(ptrCharacter, ptrCharacter + dimsData.getElementCount());
            return true;
        } break;
        case NLS_GO_HANDLE:
        case NLS_HANDLE:
        default: { } break; }
        return false;
    }
    //=============================================================================
    template <class T>
    void
    eigenSparseToIJV(const Eigen::SparseMatrix<T, 0, signedIndexType>& M, std::vector<uint64>& I,
        std::vector<uint64>& J, std::vector<T>& V, uint64& nzmax)
    {
        I.reserve(M.nonZeros());
        J.reserve(M.nonZeros());
        V.reserve(M.nonZeros());
        nzmax = M.data().allocatedSize();
        for (int i = 0; i < M.outerSize(); i++) {
            for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(M, i); it;
                 ++it) {
                I.push_back(it.row());
                J.push_back(it.col());
                V.push_back(it.value());
            }
        }
    }
    template <class T>
    void*
    IJVToAllocatedEigenSparse(const std::vector<uint64>& I, const std::vector<uint64>& J,
        const std::vector<T>& V, uint64 cols, uint64 rows, uint64 nzmax)
    {
        std::vector<Eigen::Triplet<T>> tripletList;
        size_t sizeI = I.size();
        tripletList.reserve(sizeI);
        if (sizeI == J.size() && J.size() == V.size() && sizeI != 0) {
            for (indexType k = 0; k < sizeI; ++k) {
                tripletList.push_back(Eigen::Triplet<T>(I[k], J[k], V[k]));
            }
            Eigen::SparseMatrix<T, 0, signedIndexType>* spMat;
            try {
                spMat = new Eigen::SparseMatrix<T, 0, signedIndexType>(cols, rows);

            } catch (const std::bad_alloc&) {
                spMat = nullptr;
                Error(ERROR_MEMORY_ALLOCATION);
            }
            if (spMat) {
                spMat->setFromTriplets(tripletList.begin(), tripletList.end());
                spMat->reserve(nzmax);
                spMat->makeCompressed();
                spMat->data().squeeze();
                return (void*)spMat;
            }
        }
        return nullptr;
    }
    //=============================================================================
    ArrayOf
    get(bool& success)
    {
        ArrayOf res;
        Class destinationClass = (Class)nelsonObjectClass;
        Dimensions destinationDims(dims.size());
        for (size_t k = 0; k < dims.size(); k++) {
            destinationDims[k] = dims[k];
        }
        switch (destinationClass) {
        case NLS_GO_HANDLE: {
            success = false;
        } break;
        case NLS_HANDLE: {
            success = false;
        } break;
        case NLS_CELL_ARRAY: {
            indexType nbElements = destinationDims.getElementCount();
            ArrayOf* ptrArrayOf = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
            res = ArrayOf(NLS_CELL_ARRAY, destinationDims, ptrArrayOf, isSparse);
            for (indexType k = 0; k < nbElements; ++k) {
                ptrArrayOf[k] = otherObject[k].get(success);
            }
        } break;
        case NLS_STRING_ARRAY: {
            indexType nbElements = destinationDims.getElementCount();
            ArrayOf* ptrArrayOf = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbElements);
            res = ArrayOf(NLS_STRING_ARRAY, destinationDims, ptrArrayOf, isSparse);
            for (indexType k = 0; k < nbElements; ++k) {
                ptrArrayOf[k] = otherObject[k].get(success);
            }
        } break;
        case NLS_STRUCT_ARRAY: {
            indexType nbElements = destinationDims.getElementCount();
            ArrayOf* ptrArrayOf
                = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, nbElements, fieldnames);
            res = ArrayOf(NLS_STRUCT_ARRAY, destinationDims, ptrArrayOf, isSparse, fieldnames);
            for (indexType k = 0; k < nbElements; ++k) {
                ptrArrayOf[k] = otherObject[k].get(success);
            }
        } break;
        case NLS_LOGICAL: {
            if (!isSparse) {
                uint8* ptrUInt8 = (uint8*)ArrayOf::allocateArrayOf(
                    NLS_UINT8, destinationDims.getElementCount());
                res = ArrayOf(NLS_UINT8, destinationDims, ptrUInt8, isSparse);
                memcpy(ptrUInt8, asUint8.data(), sizeof(uint8) * asUint8.size());
            } else {
                res = ArrayOf(NLS_LOGICAL, destinationDims,
                    IJVToAllocatedEigenSparse<logical>(I, J, asUint8, nzmax,
                        destinationDims.getRows(), destinationDims.getColumns()),
                    isSparse);
            }
            success = true;
        } break;
        case NLS_UINT8: {
            uint8* ptrUInt8
                = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, destinationDims.getElementCount());
            res = ArrayOf(NLS_UINT8, destinationDims, ptrUInt8, isSparse);
            memcpy(ptrUInt8, asUint8.data(), sizeof(uint8) * asUint8.size());
            success = true;
        } break;
        case NLS_INT8: {
            int8* ptrInt8
                = (int8*)ArrayOf::allocateArrayOf(NLS_INT8, destinationDims.getElementCount());
            res = ArrayOf(NLS_INT8, destinationDims, ptrInt8, isSparse);
            memcpy(ptrInt8, asInt8.data(), sizeof(int8) * asInt8.size());
            success = true;
        } break;
        case NLS_UINT16: {
            uint16* ptrUInt16
                = (uint16*)ArrayOf::allocateArrayOf(NLS_UINT16, destinationDims.getElementCount());
            res = ArrayOf(NLS_UINT16, destinationDims, ptrUInt16, isSparse);
            memcpy(ptrUInt16, asUint16.data(), sizeof(uint16) * asUint16.size());
            success = true;
        } break;
        case NLS_INT16: {
            int16* ptrInt16
                = (int16*)ArrayOf::allocateArrayOf(NLS_INT16, destinationDims.getElementCount());
            res = ArrayOf(NLS_INT16, destinationDims, ptrInt16, isSparse);
            memcpy(ptrInt16, asInt16.data(), sizeof(int16) * asInt16.size());
            success = true;
        } break;
        case NLS_UINT32: {
            uint32* ptrUInt32
                = (uint32*)ArrayOf::allocateArrayOf(NLS_UINT32, destinationDims.getElementCount());
            res = ArrayOf(NLS_UINT32, destinationDims, ptrUInt32, isSparse);
            memcpy(ptrUInt32, asUint32.data(), sizeof(uint32) * asUint32.size());
            success = true;
        } break;
        case NLS_INT32: {
            int32* ptrInt32
                = (int32*)ArrayOf::allocateArrayOf(NLS_INT32, destinationDims.getElementCount());
            res = ArrayOf(NLS_INT32, destinationDims, ptrInt32, isSparse);
            memcpy(ptrInt32, asInt32.data(), sizeof(int32) * asInt32.size());
            success = true;
        } break;
        case NLS_UINT64: {
            uint64* ptrUInt64
                = (uint64*)ArrayOf::allocateArrayOf(NLS_UINT64, destinationDims.getElementCount());
            res = ArrayOf(NLS_UINT64, destinationDims, ptrUInt64, isSparse);
            memcpy(ptrUInt64, asUint64.data(), sizeof(uint64) * asUint64.size());
            success = true;
        } break;
        case NLS_INT64: {
            int64* ptrInt64
                = (int64*)ArrayOf::allocateArrayOf(NLS_INT64, destinationDims.getElementCount());
            res = ArrayOf(NLS_INT64, destinationDims, ptrInt64, isSparse);
            memcpy(ptrInt64, asInt64.data(), sizeof(int64) * asInt64.size());
            success = true;
        } break;
        case NLS_SINGLE: {
            single* ptrSingle
                = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, destinationDims.getElementCount());
            res = ArrayOf(NLS_SINGLE, destinationDims, ptrSingle, isSparse);
            memcpy(ptrSingle, asSingle.data(), sizeof(single) * asSingle.size());
            success = true;
        } break;
        case NLS_DOUBLE: {
            if (!isSparse) {
                double* ptrDouble = (double*)ArrayOf::allocateArrayOf(
                    NLS_DOUBLE, destinationDims.getElementCount());
                res = ArrayOf(NLS_DOUBLE, destinationDims, ptrDouble, isSparse);
                memcpy(ptrDouble, asDouble.data(), sizeof(double) * asDouble.size());
            } else {
                res = ArrayOf(NLS_DOUBLE, destinationDims,
                    IJVToAllocatedEigenSparse<double>(I, J, asDouble, nzmax,
                        destinationDims.getRows(), destinationDims.getColumns()),
                    isSparse);
            }
            success = true;
        } break;
        case NLS_SCOMPLEX: {
            single* ptrSingle = (single*)ArrayOf::allocateArrayOf(
                NLS_SCOMPLEX, destinationDims.getElementCount());
            res = ArrayOf(NLS_SCOMPLEX, destinationDims, ptrSingle, isSparse);
            memcpy(ptrSingle, asSingle.data(), sizeof(single) * asSingle.size());
            success = true;
        } break;
        case NLS_DCOMPLEX: {
            if (!isSparse) {
                double* ptrDouble = (double*)ArrayOf::allocateArrayOf(
                    NLS_DCOMPLEX, destinationDims.getElementCount());
                res = ArrayOf(NLS_DCOMPLEX, destinationDims, ptrDouble, isSparse);
                memcpy(ptrDouble, asDouble.data(), sizeof(double) * asDouble.size());
            } else {
                auto* Vz = reinterpret_cast<doublecomplex*>(asDouble.data());
                std::vector<doublecomplex> V;
                V.reserve(asDouble.size() / 2);
                V.assign(Vz, Vz + (asDouble.size() / 2));
                res = ArrayOf(NLS_DCOMPLEX, destinationDims,
                    IJVToAllocatedEigenSparse<doublecomplex>(
                        I, J, V, nzmax, destinationDims.getRows(), destinationDims.getColumns()),
                    isSparse);
            }
            success = true;
        } break;
        case NLS_CHAR: {
            charType* ptrCharacter = (charType*)ArrayOf::allocateArrayOf(
                NLS_CHAR, destinationDims.getElementCount(), stringVector(), true);
            res = ArrayOf(NLS_CHAR, destinationDims, ptrCharacter, isSparse);
            memcpy(ptrCharacter, asCharacter.data(), sizeof(charType) * asCharacter.size());
            success = true;
        } break;
        default: {
            success = false;
        } break;
        }
        return res;
    }
    //=============================================================================
private:
    int nelsonObjectClass;
    bool isSparse;
    std::vector<uint64> dims;
    std::vector<std::string> fieldnames;
    std::vector<int8> asInt8;
    std::vector<uint8> asUint8;
    std::vector<int16> asInt16;
    std::vector<uint16> asUint16;
    std::vector<int32> asInt32;
    std::vector<uint32> asUint32;
    std::vector<int64> asInt64;
    std::vector<uint64> asUint64;
    std::vector<double> asDouble;
    std::vector<single> asSingle;
    std::vector<charType> asCharacter;
    uint64 nzmax;
    std::vector<uint64> I;
    std::vector<uint64> J;
    std::vector<nelsonObject> otherObject;
    //=============================================================================
    friend class boost::serialization::access;
    //=============================================================================
    template <class Archive>
    void
    serialize(Archive& ar, const unsigned int version)
    {
        ar& nelsonObjectClass;
        ar& isSparse;
        ar& dims;
        ar& fieldnames;
        if (isSparse) {
            ar& I;
            ar& J;
            ar& nzmax;
        }
        switch ((Class)nelsonObjectClass) {
        case NLS_NOT_TYPED:
        case NLS_GO_HANDLE:
        case NLS_HANDLE:
        default: {
        } break;
        case NLS_CELL_ARRAY:
        case NLS_STRUCT_ARRAY:
        case NLS_STRING_ARRAY: {
            ar& otherObject;
        } break;
        case NLS_LOGICAL:
        case NLS_UINT8: {
            ar& asUint8;
        } break;
        case NLS_INT8: {
            ar& asInt8;
        } break;
        case NLS_UINT16: {
            ar& asUint16;
        } break;
        case NLS_INT16: {
            ar& asInt16;
        } break;
        case NLS_UINT32: {
            ar& asUint32;
        } break;
        case NLS_INT32: {
            ar& asInt32;
        } break;
        case NLS_UINT64: {
            ar& asUint64;
        } break;
        case NLS_INT64: {
            ar& asInt64;
        } break;
        case NLS_SINGLE: {
            ar& asSingle;
        } break;
        case NLS_DOUBLE: {
            ar& asDouble;
        } break;
        case NLS_SCOMPLEX: {
            ar& asSingle;
        } break;
        case NLS_DCOMPLEX: {
            ar& asDouble;
        } break;
        case NLS_CHAR: {
            ar& asCharacter;
        } break;
        }
    }
};
//=============================================================================
class dataInterProcessToExchange
{
public:
    dataInterProcessToExchange(const std::string& _lineToEvaluate)
        : commandType("eval")
        , lineToEvaluate(_lineToEvaluate)
        , variable(ArrayOf())
        , variableName("")
        , scope(""){};
    dataInterProcessToExchange(
        const std::string& _variableName, const std::string& _scope, const ArrayOf& data)
        : commandType("put"), variable(data), variableName(_variableName), scope(_scope){};

    nelsonObject variable;
    std::string commandType;
    std::string lineToEvaluate;
    std::string variableName;
    std::string scope;

    void
    clear()
    {
        variable.clear();
        commandType.clear();
        lineToEvaluate.clear();
        variableName.clear();
        scope.clear();
    }

private:
    friend class boost::serialization::access;

    template <class Archive>
    void
    serialize(Archive& ar, const unsigned int version)
    {
        ar& commandType;
        if (commandType == "eval") {
            ar& lineToEvaluate;
        }
        if (commandType == "put") {
            ar& variable;
            ar& variableName;
            ar& scope;
        }
    }
};
//=============================================================================
static std::string
getChannelName(int currentPID)
{
    return std::string(NELSON_COMMAND_INTERPROCESS) + "_" + std::to_string(currentPID);
}
//=============================================================================
static void
createNelsonInterprocessReceiverThread(int currentPID)
{
    receiverLoopRunning = true;
    boost::interprocess::message_queue::remove(getChannelName(currentPID).c_str());
    try {
        boost::interprocess::message_queue messages(boost::interprocess::create_only,
            getChannelName(currentPID).c_str(), MAX_NB_MSG, MAX_MSG_SIZE);

        while (receiverLoopRunning) {
            unsigned int priority = 0;
            size_t recvd_size = 0;
            dataInterProcessToExchange msg("");
            std::string serialized_compressed_string;
            serialized_compressed_string.resize(MAX_MSG_SIZE);
            std::stringstream iss;
            if (messages.try_receive(
                    &serialized_compressed_string[0], MAX_MSG_SIZE, recvd_size, priority)) {
                if (recvd_size != 0) {
                    serialized_compressed_string[recvd_size] = 0;
                    bool failed = false;
                    std::string decompressed_string
                        = decompressString(serialized_compressed_string, failed);
                    serialized_compressed_string.clear();
                    if (!failed) {
                        iss << decompressed_string;
                        try {
                            boost::archive::binary_iarchive ia(iss);
                            ia >> msg;
                            if (msg.commandType == "eval") {
                                postCommand(utf8_to_wstring(msg.lineToEvaluate));
                            } else if (msg.commandType == "put") {
                                Evaluator* eval = getMainEvaluator();
                                if (eval) {
                                    Context* context = eval->getContext();
                                    Scope* scope = nullptr;
                                    if (msg.scope == "global") {
                                        scope = context->getGlobalScope();
                                    }
                                    if (msg.scope == "base") {
                                        scope = context->getBaseScope();
                                    }
                                    if (msg.scope == "caller") {
                                        scope = context->getCallerScope();
                                    }
                                    if (msg.scope == "local") {
                                        scope = context->getCurrentScope();
                                    }
                                    if (scope != nullptr) {
                                        bool success;
                                        ArrayOf var = msg.variable.get(success);
                                        scope->insertVariable(msg.variableName, var);
                                        msg.clear();
                                    }
                                }
                            } else {
                            }
                        } catch (boost::archive::archive_exception& e) {
                            e;
                        }
                    }
                }
            }
            try {
                boost::this_thread::sleep(boost::posix_time::milliseconds(200));
            } catch (boost::thread_interrupted&) {
                return;
            }
        }

        receiverLoopRunning = false;

    } catch (boost::interprocess::interprocess_exception&) {
        removeNelsonInterprocessReceiver(currentPID);
        receiverLoopRunning = false;
    }
    removeNelsonInterprocessReceiver(currentPID);
}
//=============================================================================
void
createNelsonInterprocessReceiver(int pid)
{
    receiver_thread = new boost::thread(createNelsonInterprocessReceiverThread, pid);
    receiver_thread->detach();
}
//=============================================================================
bool
removeNelsonInterprocessReceiver(int pid)
{
    if (receiver_thread) {
        receiverLoopRunning = false;
        receiver_thread->interrupt();
        receiver_thread = nullptr;
    }
    return boost::interprocess::message_queue::remove(getChannelName(pid).c_str());
}
//=============================================================================
static bool calledOnce = false;
// UGLY workaround, need to leave time to create temp file.
//=============================================================================
bool
sendCommandToNelsonInterprocessReceiver(int pidDestination, const std::wstring& command)
{
    if (!calledOnce) {
        boost::this_thread::sleep(boost::posix_time::seconds(10));
        calledOnce = true;
    }
    dataInterProcessToExchange msg(wstring_to_utf8(command));
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    if (failed) {
        return false;
    }
    bool bSend = false;
    try {
        boost::interprocess::message_queue messages(
            boost::interprocess::open_only, getChannelName(pidDestination).c_str());
        messages.send(serialized_compressed_string.data(), serialized_compressed_string.size(), 0);
        bSend = true;
    } catch (boost::interprocess::interprocess_exception&) {
        bSend = false;
    }
    return bSend;
}
//=============================================================================
bool
sendVariableToNelsonInterprocessReceiver(
    int pidDestination, const ArrayOf& var, const std::wstring& name, const std::wstring& scope)
{
    if (!calledOnce) {
        boost::this_thread::sleep(boost::posix_time::seconds(10));
        calledOnce = true;
    }
    dataInterProcessToExchange msg(wstring_to_utf8(name), wstring_to_utf8(scope), var);
    std::stringstream oss;
    boost::archive::binary_oarchive oa(oss);
    oa << msg;
    bool failed = false;
    std::string serialized_compressed_string = compressString(oss.str(), failed);
    bool bSend = false;
    if (failed) {
        return bSend;
    }
    if (serialized_compressed_string.size() < MAX_MSG_SIZE) {
        try {
            boost::interprocess::message_queue messages(
                boost::interprocess::open_only, getChannelName(pidDestination).c_str());
            messages.send(
                serialized_compressed_string.data(), serialized_compressed_string.size(), 0);
            bSend = true;
        } catch (boost::interprocess::interprocess_exception&) {
            bSend = false;
        }
    }
    return bSend;
}
//=============================================================================
}
//=============================================================================
