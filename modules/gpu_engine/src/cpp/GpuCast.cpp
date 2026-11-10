//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GpuKernels.hpp"
#include "GpuDispatch.hpp"
#include "GpuContext.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// WGSL that reads element i of `a` as an f32 value `x` (raw 32-bit integers are decoded)
static std::string
loadAsFloat(GpuStorage storage)
{
    switch (storage) {
    case GpuStorage::Int32:
        return "        let x = f32(bitcast<i32>(a[i]));\n";
    case GpuStorage::Uint32:
        return "        let x = f32(bitcast<u32>(a[i]));\n";
    default:
        return "        let x = a[i];\n";
    }
}
//=============================================================================
// WGSL body converting element i to `target`, writing c[i]
static std::string
castBody(GpuStorage source, const std::string& target)
{
    // NaN detection that survives fast-math compilers
    const std::string isNan = "((bitcast<u32>(x) & 0x7fffffffu) > 0x7f800000u)";
    // round half away from zero, as the host conversion does
    const std::string rounded = "(sign(x) * floor(abs(x) + 0.5))";
    if (target == "single" || target == "logical") {
        std::string value = (target == "logical") ? "f32(x != 0.0)" : "x";
        return loadAsFloat(source) + "        c[i] = " + value + ";\n";
    }
    if (target == "int32" || target == "uint32") {
        bool toUnsigned = (target == "uint32");
        // integer to integer stays exact: only the sign range differs
        if (source == GpuStorage::Int32 && toUnsigned) {
            return "        c[i] = bitcast<f32>(u32(max(bitcast<i32>(a[i]), 0)));\n";
        }
        if (source == GpuStorage::Uint32 && !toUnsigned) {
            return "        c[i] = bitcast<f32>(i32(min(bitcast<u32>(a[i]), 2147483647u)));\n";
        }
        std::string T = toUnsigned ? "u32" : "i32";
        std::string lo = toUnsigned ? "0.0" : "-2147483648.0";
        std::string hi = toUnsigned ? "4294967295.0" : "2147483647.0";
        std::string minValue = toUnsigned ? "0u" : "(-2147483647 - 1)";
        std::string maxValue = toUnsigned ? "4294967295u" : "2147483647";
        return loadAsFloat(source) + "        var r : " + T + " = " + T
            + "(0);\n"
              "        if (!"
            + isNan + ") {\n            let v = " + rounded + ";\n            if (v >= " + hi
            + ") { r = " + maxValue + "; }\n            else if (v <= " + lo + ") { r = " + minValue
            + "; }\n            else { r = " + T
            + "(v); }\n        }\n        c[i] = bitcast<f32>(r);\n";
    }
    // int8/uint8/int16/uint16 are stored as exact f32 values
    NelsonType hostClass = NLS_SINGLE;
    std::string lo, hi;
    GpuIntegerUnderlyingType(target, hostClass, lo, hi);
    return loadAsFloat(source) + "        c[i] = select(clamp(" + rounded + ", " + lo + ", " + hi
        + "), 0.0, " + isNan + ");\n";
}
//=============================================================================
static bool
anyNan(GpuArrayObject* a)
{
    ArrayOf flags = GpuUnaryElementwise(
        a, "gpuArray:isnanbits", "f32((bitcast<u32>(ai) & 0x7fffffffu) > 0x7f800000u)", "logical");
    GpuArrayObject* flagsObject = AsGpuArrayObject(flags);
    // reshape to a column so one reduction covers every element
    ArrayOf column = GpuReshape(flagsObject, Dimensions(flagsObject->elementCount(), 1));
    ArrayOf found = GatherGpuArray(AsGpuArrayObject(GpuAny(AsGpuArrayObject(column), 1)));
    return found.isScalar() && found.asLogicalScalar();
}
//=============================================================================
ArrayOf
GpuCast(const ArrayOf& value, const std::string& targetClass)
{
    GpuArrayObject* a = AsGpuArrayObject(value);
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    // the device has no f64: double keeps single storage like gpuArray(double)
    std::string target = (targetClass == "double") ? "single" : targetClass;
    NelsonType smallClass = NLS_SINGLE;
    std::string lo, hi;
    bool supported = target == "single" || target == "logical" || target == "int32"
        || target == "uint32" || GpuIntegerUnderlyingType(target, smallClass, lo, hi);
    if (!supported) {
        Error(std::string("Nelson:gpuArray:castUnsupported"),
            std::string("Conversion of a gpuArray to %s is not supported."), targetClass);
    }
    if (a->underlyingType() == target) {
        return value;
    }
    if (a->isComplex()) {
        if (target == "single") {
            return value;
        }
        if (target == "logical") {
            Error(L"Nelson:gpuArray:logicalComplex",
                L"Complex values cannot be converted to logical.");
        }
        Error(L"Nelson:gpuArray:complexIntegerUnsupported",
            L"Complex integer gpuArray values are not supported.");
    }
    GpuStorage storage = a->storageKind();
    bool floatSource = (storage == GpuStorage::Float32) && a->underlyingType() == "single";
    if (target == "logical" && floatSource && anyNan(a)) {
        Error(L"Nelson:gpuArray:logicalNaN", L"NaN values cannot be converted to logical.");
    }
    std::string source = std::string("struct Params { n : u32 };\n"
                                     "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                                     "@group(0) @binding(1) var<storage, read_write> c : "
                                     "array<f32>;\n"
                                     "@group(0) @binding(2) var<uniform> params : Params;\n"
                                     "@compute @workgroup_size(256)\n"
                                     "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                                     "    let i = gid.x;\n"
                                     "    if (i < params.n) {\n")
        + castBody(storage, target) + "    }\n}\n";
    std::string key = "gpuArray:cast:" + a->underlyingType() + ":" + target;
    WGPUComputePipeline pipeline
        = GpuContext::getInstance()->pipelineFor(key, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(a->dimensions(), target, false);
    uint32_t n = (uint32_t)a->elementCount();
    if (n > 0) {
        uint32_t params[1] = { n };
        try {
            GpuDispatchKernel(
                pipeline, { a->buffer(), output->buffer() }, params, 1, (n + 255u) / 256u, 1);
        } catch (...) {
            delete output;
            throw;
        }
    }
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
