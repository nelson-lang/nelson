//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "JuliaHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
jl_value_tRepresentation(jl_value_t* value, bool& fails, int nbRows, int nbColumns)
{
    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
    if (!jl_base_module) {
        fails = true;
        return "";
    }

    jl_value_t* iobuffer = NLSjl_eval_string("IOBuffer()");
    if (!iobuffer) {
        fails = true;
        return "";
    }

    jl_function_t* show_func = NLSjl_get_function(jl_base_module, "show");
    if (!show_func) {
        fails = true;
        return "";
    }

    jl_value_t* mime = NLSjl_eval_string("MIME(\"text/plain\")");
    if (!mime) {
        fails = true;
        return "";
    }

    jl_function_t* io_context_func = NLSjl_get_function(jl_base_module, "IOContext");
    if (!io_context_func) {
        fails = true;
        return "";
    }
    jl_value_t* limit_value = NLSjl_eval_string(":limit=>true");
    std::string displaysize_str = fmt::sprintf(":displaysize=>(%d,%d)", nbColumns, nbRows);
    jl_value_t* displaysize_value = NLSjl_eval_string(displaysize_str.c_str());

    jl_value_t* io_context = NLSjl_call3(io_context_func, iobuffer, limit_value, displaysize_value);
    if (!io_context) {
        fails = true;
        return "";
    }

    NLSjl_call3(show_func, io_context, mime, value);
    if (NLSjl_exception_occurred()) {
        fails = true;
        return "";
    }

    jl_function_t* string_func = NLSjl_get_function(jl_base_module, "String");
    jl_function_t* take_func = NLSjl_get_function(jl_base_module, "take!");
    if (!string_func || !take_func) {
        fails = true;
        return "";
    }

    jl_value_t* buffer_content = NLSjl_call1(take_func, iobuffer);
    if (NLSjl_exception_occurred() || !buffer_content) {
        fails = true;
        return "";
    }

    jl_value_t* result = NLSjl_call1(string_func, buffer_content);
    if (NLSjl_exception_occurred() || !result) {
        fails = true;
        return "";
    }
    fails = false;
    return std::string(NLSjl_string_ptr(result));
}
//=============================================================================
}
//=============================================================================
