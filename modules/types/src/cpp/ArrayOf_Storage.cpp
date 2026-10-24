//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4724)
#endif
//=============================================================================
#include <cwchar>
#include <cstring>
#include "lapack_eigen_config.hpp"
#include "omp_for_loop.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <algorithm>
#include <cinttypes>
#include <cmath>
#include <cstdio>
#include <limits>
#include <utility>
#include "StringHelpers.hpp"
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "HandleState.hpp"
#include "IEEEFP.hpp"
#include "SparseType.hpp"
#include "characters_encoding.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
//=============================================================================
#ifdef _MSC_VER
#define snprintf _snprintf
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSingletonShape(const Dimensions& dims)
{
    if (dims.length() == 0 || dims.length() > maxDims) {
        return false;
    }
    for (indexType k = 0; k < dims.length(); ++k) {
        if (dims.at(k) != 1) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
canUseScalarStorage(NelsonType type, const Dimensions& dims, const void* data, bool sparse,
    const stringVector& fields)
{
    return (type == NLS_DOUBLE || type == NLS_SINGLE || type == NLS_LOGICAL) && !sparse && data
        && fields.empty() && (dims.length() == 2 ? dims.isScalar() : isSingletonShape(dims));
}
//=============================================================================
bool
ArrayOf::canUseScalarData() const
{
    return dp && !dp->sparse && dp->isScalar()
        && canUseScalarStorage(
            dp->dataClass, dp->dimensions(), dp->data(), dp->sparse, dp->fieldNames());
}
//=============================================================================
ArrayOfVector
scalarArrayOfToArrayOfVector(const ArrayOf& a)
{
    ArrayOfVector retval(1);
    retval << a;
    return retval;
}
//=============================================================================
std::wstring
ArrayOf::wname() const
{
    return utf8_to_wstring(_name);
}
//=============================================================================
std::string
ArrayOf::name() const
{
    return _name;
}
//=============================================================================
void
ArrayOf::name(const std::string& name)
{
    _name = name;
}
//=============================================================================
bool
ArrayOf::hasScalarStorage() const noexcept
{
    return representation_ != Representation::DataRef;
}
//=============================================================================
size_t
ArrayOf::scalarStorageSize(NelsonType type) noexcept
{
    switch (type) {
    case NLS_DOUBLE:
        return sizeof(double);
    case NLS_SINGLE:
        return sizeof(single);
    case NLS_LOGICAL:
        return sizeof(logical);
    default:
        return 0;
    }
}
//=============================================================================
const void*
ArrayOf::scalarDataPointer() const noexcept
{
    return representation_ == Representation::OwnedScalar ? scalarStorage_.owned : &scalarStorage_;
}
//=============================================================================
void
ArrayOf::setInlineScalar(NelsonType type, const void* value, indexType dimensionCount) noexcept
{
    dp = nullptr;
    representation_ = Representation::InlineScalar;
    scalarType_ = static_cast<uint8_t>(type);
    scalarDimensionCount_ = static_cast<uint8_t>(dimensionCount);
    switch (type) {
    case NLS_LOGICAL:
        scalarStorage_.logicalValue = 0;
        std::memcpy(&scalarStorage_.logicalValue, value, sizeof(logical));
        break;
    case NLS_SINGLE:
        scalarStorage_.singleValue = 0.f;
        std::memcpy(&scalarStorage_.singleValue, value, sizeof(single));
        break;
    default:
        scalarStorage_.doubleValue = 0.;
        std::memcpy(&scalarStorage_.doubleValue, value, sizeof(double));
        break;
    }
}
//=============================================================================
void
ArrayOf::setOwnedScalar(NelsonType type, void* data, indexType dimensionCount) noexcept
{
    dp = nullptr;
    scalarStorage_.owned = data;
    representation_ = Representation::OwnedScalar;
    scalarType_ = static_cast<uint8_t>(type);
    scalarDimensionCount_ = static_cast<uint8_t>(dimensionCount);
}
//=============================================================================
void
ArrayOf::materializeScalar()
{
    if (!hasScalarStorage()) {
        return;
    }
    const Dimensions dims = dimensions();
    const auto type = static_cast<NelsonType>(scalarType_);
    Data* data = representation_ == Representation::OwnedScalar
        ? new Data(type, dims, scalarStorage_.owned)
        : new Data(type, dims, scalarDataPointer(), scalarStorageSize(type));
    representation_ = Representation::DataRef;
    scalarStorage_.doubleValue = 0.;
    dp = data;
}
//=============================================================================
bool
ArrayOf::tryAdoptScalarStorage(
    NelsonType type, const Dimensions& dims, void* data, bool sparse, const stringVector& fields)
{
    if (!canUseScalarStorage(type, dims, data, sparse, fields)) {
        return false;
    }
    const auto dimensionCount = static_cast<uint8_t>(dims.length());
    if (representation_ == Representation::OwnedScalar && scalarStorage_.owned == data
        && scalarType_ == type) {
        scalarDimensionCount_ = dimensionCount;
        return true;
    }
    deleteContents();
    setOwnedScalar(type, data, dimensionCount);
    return true;
}
//=============================================================================
void
ArrayOf::replaceData(
    NelsonType type, const Dimensions& dims, void* data, bool sparse, const stringVector& fields)
{
    if (tryAdoptScalarStorage(type, dims, data, sparse, fields)) {
        return;
    }
    materializeScalar();
    dp = dp->putData(type, dims, data, sparse, fields);
}
//=============================================================================
void
ArrayOf::copyObject(const ArrayOf& copy)
{
    this->_name = copy._name;
    if (copy.hasScalarStorage()) {
        if (copy.representation_ == Representation::OwnedScalar) {
            setInlineScalar(static_cast<NelsonType>(copy.scalarType_), copy.scalarStorage_.owned,
                copy.scalarDimensionCount_);
        } else {
            dp = nullptr;
            representation_ = Representation::InlineScalar;
            scalarType_ = copy.scalarType_;
            scalarDimensionCount_ = copy.scalarDimensionCount_;
            scalarStorage_ = copy.scalarStorage_;
        }
    } else if (copy.dp && copy.dp->isScalar() && copy.canUseScalarData()) {
        setInlineScalar(copy.dp->dataClass, copy.dp->data(), copy.nDims());
    } else {
        representation_ = Representation::DataRef;
        dp = copy.dp ? copy.dp->copy() : nullptr;
    }
}
//=============================================================================
void
ArrayOf::moveObject(ArrayOf&& move) noexcept
{
    _name = std::move(move._name);
    if (move.canUseScalarData() && move.dp->numberOfOwners() == 1
        && move.dp->allocatedElementCount == 1) {
        const indexType dimensionCount = move.nDims();
        const NelsonType type = move.dp->dataClass;
        if (move.dp->isInline) {
            unsigned char value[sizeof(ScalarStorage)];
            std::memcpy(value, move.dp->data(), scalarStorageSize(type));
            move.deleteContents();
            setInlineScalar(type, value, dimensionCount);
        } else {
            // Transfer the buffer without reading it: callers may still be filling it.
            void* data = move.dp->writableData();
            move.dp->cp = nullptr;
            move.deleteContents();
            setOwnedScalar(type, data, dimensionCount);
        }
        return;
    }
    dp = move.dp;
    representation_ = move.representation_;
    scalarDimensionCount_ = move.scalarDimensionCount_;
    scalarType_ = move.scalarType_;
    scalarStorage_ = move.scalarStorage_;
    move.dp = nullptr;
    move.representation_ = Representation::DataRef;
    move.scalarStorage_.doubleValue = 0.;
}
//=============================================================================
void
ArrayOf::deleteContents()
{
    if (representation_ == Representation::InlineScalar) {
        representation_ = Representation::DataRef;
        scalarStorage_.doubleValue = 0.;
        return;
    }
    if (representation_ == Representation::OwnedScalar) {
        switch (scalarType_) {
        case NLS_LOGICAL:
            delete[] static_cast<logical*>(scalarStorage_.owned);
            break;
        case NLS_SINGLE:
            delete[] static_cast<single*>(scalarStorage_.owned);
            break;
        default:
            delete[] static_cast<double*>(scalarStorage_.owned);
            break;
        }
        representation_ = Representation::DataRef;
        scalarStorage_.doubleValue = 0.;
    } else if (dp) {
        if (dp->deleteCopy() == 1) {
            delete dp;
        }
    }
    dp = nullptr;
}
//=============================================================================
ArrayOf::ArrayOf()
{
    representation_ = Representation::DataRef;
    dp = new Data(NLS_DOUBLE, Dimensions(0, 0), nullptr);
}
//=============================================================================
ArrayOf::ArrayOf(ArrayOf&& move) noexcept { moveObject(std::move(move)); }
//=============================================================================
ArrayOf::ArrayOf(
    NelsonType type, const Dimensions& dims, void* data, bool sparse, const stringVector& fnames)
{
    if (canUseScalarStorage(type, dims, data, sparse, fnames)) {
        setOwnedScalar(type, data, dims.length());
        return;
    }
    representation_ = Representation::DataRef;
    dp = new Data(type, dims, data, sparse, fnames);
}
//=============================================================================
ArrayOf::ArrayOf(NelsonType type, const Dimensions& dims, const void* scalarData, size_t dataSize)
{
    if (dataSize == scalarStorageSize(type)
        && canUseScalarStorage(type, dims, scalarData, false, stringVector())) {
        setInlineScalar(type, scalarData, dims.length());
        return;
    }
    representation_ = Representation::DataRef;
    dp = new Data(type, dims, scalarData, dataSize);
}
//=============================================================================
ArrayOf::ArrayOf(NelsonType type)
{
    representation_ = Representation::DataRef;
    dp = new Data(type, Dimensions(0, 0), nullptr);
}
//=============================================================================
void
ArrayOf::operator=(const ArrayOf& copy)
{
    if (this == &copy) {
        return;
    }
    this->_name = copy._name;
    deleteContents();
    copyObject(copy);
}
//=============================================================================
void
ArrayOf::operator=(ArrayOf&& move) noexcept
{
    if (this == &move) {
        return;
    }
    deleteContents();
    moveObject(std::move(move));
}
//=============================================================================
ArrayOf::~ArrayOf() { deleteContents(); }
//=============================================================================
indexType
ArrayOf::referenceCount() const
{
    if (hasScalarStorage()) {
        return 1;
    }
    if (dp) {
        return dp->numberOfOwners();
    }
    return 0;
}
//=============================================================================
const void*
ArrayOf::dataPointer() const
{
    if (hasScalarStorage()) {
        return scalarDataPointer();
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported:operationDoesNotSupportSparseMatrix",
            L"operation does not support sparse matrix arguments.");
    }
    if (dp) {
        return dp->data();
    }
    return nullptr;
}
//=============================================================================
ArrayOf
ArrayOf::clone() const
{
    return ArrayOf(*this);
}
//=============================================================================
void
ArrayOf::ensureSingleOwner()
{
    if (hasScalarStorage()) {
        return;
    }
    if (dp != nullptr && dp->numberOfOwners() > 1) {
        if (!dp->sparse) {
            std::string currentClassType = dp->classTypeName();
            const stringVector& fieldNames = dp->fieldNames();
            void* np = allocateArrayOf(dp->dataClass, elementCount(), fieldNames, false);
            if (isEmpty()) {
                Dimensions dim = dp->dimensions();
                dp = dp->putData(dp->dataClass, dim, np, dp->sparse, fieldNames);
            } else {
                copyElements(0, np, 0, elementCount());
                dp = dp->putData(dp->dataClass, dp->dimensions_, np, dp->sparse, fieldNames);
            }
            dp->setClassTypeName(currentClassType);
        } else {
            indexType requestedSparseNzmax = dp->sparseNzmax;
            dp = dp->putData(dp->dataClass, dp->dimensions_,
                Eigen_CopySparseMatrix(
                    dp->dataClass, dp->dimensions_[0], dp->dimensions_[1], dp->data()),
                dp->sparse, dp->fieldNames());
            dp->sparseNzmax = requestedSparseNzmax;
        }
    }
}
//=============================================================================
void*
ArrayOf::readWriteDataPointer()
{
    if (!hasScalarStorage() && isSparse()) {
        Warning(L"Nelson:sparseConvertedToFull",
            L"Warning: sparse matrix converted to full for operation.");
        makeDense();
    }
    if (hasScalarStorage()) {
        return const_cast<void*>(scalarDataPointer());
    }
    ensureSingleOwner();
    return dp->writableData();
}
//=============================================================================
void
ArrayOf::setDataPointer(void* rp)
{
    replaceData(dataClass(), dimensions(), rp, isSparse(), fieldNames());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
