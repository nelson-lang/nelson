//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GpuArrayObject.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GpuArrayObject::GpuArrayObject(
    WGPUBuffer buffer, const Dimensions& dims, const std::string& underlyingType, bool complex)
    : HandleGenericObject(NLS_HANDLE_GPUARRAY_CATEGORY_STR, this, false)
    , m_buffer(buffer)
    , m_dimensions(dims)
    , m_underlyingType(underlyingType)
    , m_complex(complex)
{
}
//=============================================================================
GpuArrayObject::~GpuArrayObject() { releaseBuffer(); }
//=============================================================================
void
GpuArrayObject::onDelete(Evaluator* eval)
{
    releaseBuffer();
}
//=============================================================================
void
GpuArrayObject::releaseBuffer()
{
    if (m_buffer != nullptr) {
        wgpuBufferDestroy(m_buffer);
        wgpuBufferRelease(m_buffer);
        m_buffer = nullptr;
    }
}
//=============================================================================
GpuArrayObject*
AsGpuArrayObject(const ArrayOf& value)
{
    if (!value.isHandle() || !value.isScalar()) {
        return nullptr;
    }
    HandleGenericObject* genericObject = value.asHandleScalar();
    if (genericObject == nullptr
        || genericObject->categoryName() != NLS_HANDLE_GPUARRAY_CATEGORY_STR) {
        return nullptr;
    }
    return static_cast<GpuArrayObject*>(genericObject);
}
//=============================================================================
bool
GpuIntegerUnderlyingType(const std::string& underlyingType, NelsonType& hostClass,
    std::string& wgslLo, std::string& wgslHi)
{
    if (underlyingType == "int8") {
        hostClass = NLS_INT8;
        wgslLo = "-128.0";
        wgslHi = "127.0";
        return true;
    }
    if (underlyingType == "uint8") {
        hostClass = NLS_UINT8;
        wgslLo = "0.0";
        wgslHi = "255.0";
        return true;
    }
    if (underlyingType == "int16") {
        hostClass = NLS_INT16;
        wgslLo = "-32768.0";
        wgslHi = "32767.0";
        return true;
    }
    if (underlyingType == "uint16") {
        hostClass = NLS_UINT16;
        wgslLo = "0.0";
        wgslHi = "65535.0";
        return true;
    }
    return false;
}
//=============================================================================
static bool
gpuIsIntegerType(const std::string& underlyingType)
{
    NelsonType hostClass = NLS_SINGLE;
    std::string lo, hi;
    return GpuIntegerUnderlyingType(underlyingType, hostClass, lo, hi);
}
//=============================================================================
std::string
GpuIntegerResultType(GpuArrayObject* a, GpuArrayObject* b)
{
    std::string ta = a->underlyingType();
    std::string tb = b->underlyingType();
    bool ia = gpuIsIntegerType(ta);
    bool ib = gpuIsIntegerType(tb);
    if (ia && ib) {
        if (ta != tb) {
            Error(L"Nelson:gpuArray:integerTypeMismatch", L"Integer types must match.");
        }
        return ta;
    }
    if (ia) {
        return ta;
    }
    if (ib) {
        return tb;
    }
    return "";
}
//=============================================================================
std::string
GpuSaturateExpr(const std::string& expr, const std::string& intType)
{
    NelsonType hostClass = NLS_SINGLE;
    std::string lo, hi;
    GpuIntegerUnderlyingType(intType, hostClass, lo, hi);
    std::string rounded = "(sign(" + expr + ") * floor(abs(" + expr + ") + 0.5))";
    return "clamp(" + rounded + ", " + lo + ", " + hi + ")";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
