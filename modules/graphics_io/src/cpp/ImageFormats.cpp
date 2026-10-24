//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include "ImageFormats.hpp"
#include "RasterCodec.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    const stringVector fieldNames
        = { "ext", "isa", "info", "read", "write", "alpha", "description", "multipage" };
    //=============================================================================
    ArrayOf
    functionHandle(Evaluator* eval, const char* name)
    {
        if (name == nullptr || *name == '\0') {
            return ArrayOf::empty(0, 0);
        }
        FunctionDef* definition = nullptr;
        if (!eval->getContext()->lookupFunction("str2func", definition)) {
            return ArrayOf::empty(0, 0);
        }
        ArrayOfVector result = definition->evaluateFunction(
            eval, ArrayOfVector { ArrayOf::characterArray(std::string(name)) }, 1);
        return result.empty() ? ArrayOf::empty(0, 0) : result[0];
    }
    //=============================================================================
    ArrayOf
    formatStruct(Evaluator* eval, const RasterFormatInfo& format)
    {
        ArrayOfVector values;
        values << ArrayOf::characterArray(format.extension) << ArrayOf::empty(0, 0)
               << ArrayOf::empty(0, 0) << functionHandle(eval, format.readable ? "imread" : nullptr)
               << functionHandle(eval, format.writable ? "imwrite" : nullptr)
               << ArrayOf::logicalScalar(format.alpha)
               << ArrayOf::characterArray(format.description)
               << ArrayOf::logicalScalar(format.multipage);
        return ArrayOf::structScalar(fieldNames, values);
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
imageSupport(Evaluator*)
{
    ArrayOfVector exts, isas, infos, reads, writes, alphas, descriptions, multipages;
    for (const auto& format : rasterFormats()) {
        exts << ArrayOf::characterArray(format.extension);
        isas << ArrayOf::characterArray("");
        infos << ArrayOf::characterArray("");
        reads << ArrayOf::characterArray(format.readable ? "imread" : "");
        writes << ArrayOf::characterArray(format.writable ? "imwrite" : "");
        alphas << ArrayOf::logicalScalar(format.alpha);
        descriptions << ArrayOf::characterArray(format.description);
        multipages << ArrayOf::logicalScalar(format.multipage);
    }
    Dimensions dimensions(1, exts.size());
    auto* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dimensions.elementCount(), fieldNames, false));
    ArrayOf result(NLS_STRUCT_ARRAY, dimensions, elements, false, fieldNames);
    result.setFieldAsList("ext", exts);
    result.setFieldAsList("isa", isas);
    result.setFieldAsList("info", infos);
    result.setFieldAsList("read", reads);
    result.setFieldAsList("write", writes);
    result.setFieldAsList("alpha", alphas);
    result.setFieldAsList("description", descriptions);
    result.setFieldAsList("multipage", multipages);
    return result;
}
//=============================================================================
ArrayOf
imageSupport(Evaluator* eval, const std::wstring& format)
{
    const RasterFormatInfo* info = rasterFormat(wstring_to_utf8(format));
    if (info != nullptr) {
        return formatStruct(eval, *info);
    }
    Dimensions dimensions(0, 0);
    return ArrayOf::emptyStruct(stringVector(), dimensions);
}
//=============================================================================
bool
isImageFormatReadable(const std::wstring& format)
{
    const RasterFormatInfo* info = rasterFormat(wstring_to_utf8(format));
    return info != nullptr && info->readable;
}
//=============================================================================
bool
isImageFormatWritable(const std::wstring& format)
{
    const RasterFormatInfo* info = rasterFormat(wstring_to_utf8(format));
    return info != nullptr && info->writable;
}
//=============================================================================
ArrayOf
imageFormatDisplay(Evaluator* eval)
{
    if (!eval) {
        return {};
    }
    Interface* io = eval->getInterface();
    io->outputMessage("\nEXT    READ     WRITE    ALPHA  MULTIPAGE  DESCRIPTION\n");
    io->outputMessage("---------------------------------------------------------------\n");
    for (const auto& format : rasterFormats()) {
        io->outputMessage(fmt::format("{:<6} {:<8} {:<8} {:<6} {:<10} {}\n", format.extension,
            format.readable ? "imread" : "-", format.writable ? "imwrite" : "-",
            format.alpha ? "true" : "false", format.multipage ? "true" : "false",
            format.description));
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
