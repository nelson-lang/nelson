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
#include <fmt/xchar.h>
#include "JuliaHelpers.hpp"
#include "IsValidFieldname.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
jl_can_be_converted(const std::string& jlTypeStr1, const std::string& jlTypeStr2)
{
    // The evaluated type objects and the freshly allocated dummy array are unrooted
    // while convert() runs (and allocates); keep the collector off around them.
    NLSjlGcDisableGuard gcGuard;
    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");

    jl_value_t* jl_type_1 = (jl_value_t*)NLSjl_eval_string(jlTypeStr1);
    jl_value_t* jl_type_2 = (jl_value_t*)NLSjl_eval_string(jlTypeStr2);

    jl_array_t* dummy_array = NLSjl_alloc_array_2d(jl_type_1, 3, 3);
    jl_value_t* converted_value = NLSjl_call2(NLSjl_get_function(jl_base_module, "convert"),
        (jl_value_t*)jl_type_2, (jl_value_t*)dummy_array);
    return converted_value ? true : false;
}
//=============================================================================
std::string
jl_get_type_of_as_string(jl_value_t* value)
{
    // typeof()/string() below allocate; keep `result` (the typeof output) alive
    // across the string() call, which would otherwise be free to collect it.
    NLSjlGcDisableGuard gcGuard;
    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
    jl_function_t* typeof_func = NLSjl_get_function(jl_base_module, "typeof");
    jl_value_t* result = NLSjl_call1(typeof_func, value);

    jl_function_t* string_func = NLSjl_get_function(jl_base_module, "string");
    jl_value_t* str_result = NLSjl_call1(string_func, result);
    return std::string(NLSjl_string_ptr(str_result));
}
//=============================================================================
std::string
jl_value_tRepresentation(jl_value_t* value, bool& fails, int nbRows, int nbColumns)
{
    // The IOBuffer / IOContext / MIME temporaries and show() output are held in
    // bare locals across many allocating calls; keep the collector off so none of
    // them is freed before the buffer is turned into a string.
    NLSjlGcDisableGuard gcGuard;
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
    std::string displaysize_str = fmt::format(":displaysize=>({0},{1})", nbColumns, nbRows);
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
std::vector<std::string>
jl_value_tfieldnames(jl_value_t* value)
{
    std::vector<std::string> fieldNames;
    if (!value) {
        return fieldNames;
    }

    // `field_names` (the fieldnames() tuple) and the per-field values are unrooted
    // while string() is called on each in the loop below; disable collection so the
    // tuple cannot be freed out from under jl_get_nth_field().
    NLSjlGcDisableGuard gcGuard;

    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
    if (!jl_base_module) {
        return fieldNames;
    }

    jl_function_t* typeof_func = NLSjl_get_function(jl_base_module, "typeof");
    if (!typeof_func) {
        return fieldNames;
    }

    jl_value_t* type = NLSjl_call1(typeof_func, value);
    if (!type) {
        return fieldNames;
    }

    jl_function_t* fieldcount_func = NLSjl_get_function(jl_base_module, "fieldcount");
    if (!fieldcount_func) {
        return fieldNames;
    }

    jl_value_t* count_val = NLSjl_call1(fieldcount_func, type);
    if (!count_val) {
        return fieldNames;
    }
    // Do not use jl_is_long(): it compares the small-type tag baked into the
    // julia.h used at build time. Julia 1.13 inserted a `nothing` tag before the
    // integer tags, so a build against 1.12 headers no longer recognizes an Int64
    // at runtime and every field enumeration comes back empty. The runtime type
    // name is stable across versions.
    const char* countTypeName = NLSjl_typeof_str(count_val);
    bool isInt64 = countTypeName && std::string(countTypeName) == "Int64";
    bool isInt32 = countTypeName && std::string(countTypeName) == "Int32";
    if (!isInt64 && !isInt32) {
        return fieldNames;
    }
    size_t num_fields = isInt64 ? static_cast<size_t>(NLSjl_unbox_int64(count_val))
                                : static_cast<size_t>(NLSjl_unbox_int32(count_val));
    jl_function_t* fieldnames_func = NLSjl_get_function(jl_base_module, "fieldnames");
    if (!fieldnames_func) {
        return fieldNames;
    }

    jl_value_t* field_names = NLSjl_call1(fieldnames_func, type);
    if (!field_names) {
        return fieldNames;
    }

    jl_function_t* string_func = NLSjl_get_function(jl_base_module, "string");
    if (!string_func) {
        return fieldNames;
    }

    for (size_t i = 0; i < num_fields; i++) {
        jl_value_t* field = NLSjl_get_nth_field(field_names, i);
        if (!field) {
            continue;
        }
        jl_value_t* field_str = NLSjl_call1(string_func, field);
        if (!field_str) {
            continue;
        }

        const char* str_ptr = NLSjl_string_ptr(field_str);
        if (str_ptr) {
            fieldNames.push_back(str_ptr);
        }
    }

    return fieldNames;
}
//=============================================================================
enum METHOD_OR_PROPERTY_NAME_TYPE
{
    GET_METHOD_NAME,
    GET_PROPERTY_NAME,
};
//=============================================================================
static stringVector
jl_value_tgetMethodOrPropertyNames(
    jl_value_t* value, METHOD_OR_PROPERTY_NAME_TYPE methodOrPropertyType)
{
    stringVector propertyNames;
    if (!value) {
        return propertyNames;
    }

    // propertynames() on a module such as Base returns thousands of symbols. The
    // getproperty()/isa() calls below allocate and could free the unrooted
    // `properties` array mid-loop, so hold the collector off for the whole loop.
    NLSjlGcDisableGuard gcGuard;

    stringVector fieldnames = jl_value_tfieldnames(value);

    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
    if (!jl_base_module) {
        return propertyNames;
    }
    jl_value_t* function_type = NLSjl_eval_string("Function");
    if (!function_type) {
        return propertyNames;
    }

    // Check declared field type (<: Function) instead of runtime isa.
    // Robust across Julia versions (avoids Julia 1.13 closure detection issues).
    jl_value_t* field_type_check_fn = NLSjl_eval_string(
        "(obj, sym) -> (try; fieldtype(typeof(obj), sym) <: Function; catch; false; end)");

    for (const std::string& fieldname : fieldnames) {
        if (fieldname.empty()) {
            continue;
        }

        bool is_func = false;

        if (field_type_check_fn) {
            jl_sym_t* fsym = NLSjl_symbol(fieldname.c_str());
            if (fsym) {
                jl_value_t* result = NLSjl_call2(field_type_check_fn, value, (jl_value_t*)fsym);
                is_func = result && NLSjl_unbox_bool(result);
            }
        }

        if (!is_func) {
            jl_value_t* field = NLSjl_get_field(value, fieldname.c_str());
            if (field) {
                jl_value_t* is_function
                    = NLSjl_call2(NLSjl_get_function(jl_base_module, "isa"), field, function_type);
                is_func = is_function && NLSjl_unbox_bool(is_function);
            }
        }

        if (methodOrPropertyType == GET_METHOD_NAME) {
            if (is_func) {
                propertyNames.push_back(fieldname);
            }
        } else {
            if (!is_func) {
                propertyNames.push_back(fieldname);
            }
        }
    }

    jl_function_t* isa_func = NLSjl_get_function(jl_base_module, "isa");
    if (!isa_func) {
        return propertyNames;
    }

    jl_value_t* properties
        = NLSjl_call1(NLSjl_get_function(jl_base_module, "propertynames"), value);
    if (!properties) {
        return propertyNames;
    }

    jl_function_t* getproperty_func = NLSjl_get_function(jl_base_module, "getproperty");
    if (!getproperty_func) {
        return propertyNames;
    }

    std::string propertiesType = jl_get_type_of_as_string(properties);

    size_t len = StringHelpers::starts_with(propertiesType, "Vector{Symbol}")
        ? jl_array_len(properties)
        : 0;

    std::vector<std::string> names;
    names.reserve(len);
    jl_array_t* arr = (jl_array_t*)properties;
    jl_value_t** data = (jl_value_t**)jl_array_data_(arr);
    for (size_t i = 0; i < len; ++i) {
        if (data[i] != nullptr) {
            const char* str = jl_symbol_name_((jl_sym_t*)data[i]);
            if (!IsValidFieldname(str)) {
                continue;
            }
            jl_value_t* field = NLSjl_call2(getproperty_func, value, data[i]);
            if (!field) {
                continue;
            }
            jl_value_t* is_function = NLSjl_call2(isa_func, field, function_type);

            if (methodOrPropertyType == GET_METHOD_NAME) {
                if (is_function && NLSjl_unbox_bool(is_function)
                    && std::find(propertyNames.begin(), propertyNames.end(), str)
                        == propertyNames.end()) {
                    propertyNames.push_back(str);
                }
            } else {
                if (is_function && !NLSjl_unbox_bool(is_function)
                    && std::find(propertyNames.begin(), propertyNames.end(), str)
                        == propertyNames.end()) {
                    propertyNames.push_back(str);
                }
            }
        }
    }
    return propertyNames;
}
//=============================================================================
stringVector
jl_value_tgetPropertyNames(jl_value_t* value)
{
    return jl_value_tgetMethodOrPropertyNames(value, GET_PROPERTY_NAME);
}
//=============================================================================
stringVector
jl_value_tgetMethodNames(jl_value_t* value)
{
    return jl_value_tgetMethodOrPropertyNames(value, GET_METHOD_NAME);
}
//=============================================================================
bool
isModule(jl_value_t* value)
{
    if (!value) {
        return false;
    }
    NLSjlGcDisableGuard gcGuard;
    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
    if (!jl_base_module) {
        return false;
    }
    jl_function_t* isa_func = NLSjl_get_function(jl_base_module, "isa");
    if (!isa_func) {
        return false;
    }
    jl_value_t* module_type = NLSjl_eval_string("Module");
    if (!module_type) {
        return false;
    }
    jl_value_t* is_module = NLSjl_call2(isa_func, value, module_type);
    return (is_module && NLSjl_unbox_bool(is_module));
}
//=============================================================================
bool
isJuliaDataFrame(jl_value_t* value)
{
    if (!value) {
        return false;
    }
    // typeof(df) prints as "DataFrame" when DataFrames was loaded with "using" and as
    // "DataFrames.DataFrame" with "import" (as jl_ensure_dataframes does). Accept both.
    // This never loads the package: without DataFrames no such value exists.
    std::string typeOf = jl_get_type_of_as_string(value);
    return typeOf == "DataFrame" || StringHelpers::ends_with(typeOf, ".DataFrame");
}
//=============================================================================
bool
jl_ensure_dataframes()
{
    // "import" is only allowed at top level, so it is wrapped in @eval (which
    // lowers to a Core.eval call) to make it usable inside the try expression.
    jl_value_t* loaded = NLSjl_eval_string("try; @eval import DataFrames; true; catch; false; end");
    if (NLSjl_exception_occurred()) {
        NLSjl_exception_clear();
        return false;
    }
    if (!loaded || !NLSjl_unbox_bool(loaded)) {
        return false;
    }
    // Small Main helpers that hide the DataFrames-specific indexing syntax from
    // the C++ side (the "!" column selector and Symbol construction). Defining
    // them again is idempotent and cheap.
    NLSjl_eval_string("function __nelson_new_dataframe__()\n"
                      "    return DataFrames.DataFrame()\n"
                      "end");
    // A Nelson table column arrives either as an array (an N-by-1 matrix, which
    // vec() flattens to a length-N vector) or, for a single-row table, as a
    // boxed scalar (wrapped into a one-element vector).
    NLSjl_eval_string("function __nelson_df_setcol__(df, name, col)\n"
                      "    v = isa(col, AbstractArray) ? vec(col) : [col]\n"
                      "    df[!, Symbol(name)] = v\n"
                      "    return df\n"
                      "end");
    NLSjl_eval_string("function __nelson_df_names__(df)\n"
                      "    return String.(DataFrames.names(df))\n"
                      "end");
    NLSjl_eval_string("function __nelson_df_ncol__(df)\n"
                      "    return DataFrames.ncol(df)\n"
                      "end");
    NLSjl_eval_string("function __nelson_df_getcol__(df, i)\n"
                      "    return df[!, i]\n"
                      "end");
    NLSjl_eval_string("function __nelson_stringify_col__(col)\n"
                      "    return String.(string.(col))\n"
                      "end");
    // Helpers for columns whose element type is Union{Missing, T}. __kind__
    // classifies the non-missing element type (1 real, 2 complex, 3 other/text);
    // the materializers replace each missing with the matching Nelson gap: NaN
    // for a numeric column, and the mask marks the missing rows of a text column.
    NLSjl_eval_string("function __nelson_col_kind__(col)\n"
                      "    T = Base.nonmissingtype(eltype(col))\n"
                      "    if T <: Complex\n"
                      "        return 2\n"
                      "    elseif T <: Real\n"
                      "        return 1\n"
                      "    else\n"
                      "        return 3\n"
                      "    end\n"
                      "end");
    NLSjl_eval_string("function __nelson_col_realmissing__(col)\n"
                      "    return Float64[ismissing(x) ? NaN : Float64(x) for x in col]\n"
                      "end");
    NLSjl_eval_string("function __nelson_col_complexmissing__(col)\n"
                      "    return ComplexF64[ismissing(x) ? ComplexF64(NaN, NaN) : ComplexF64(x) "
                      "for x in col]\n"
                      "end");
    NLSjl_eval_string("function __nelson_col_missingmask__(col)\n"
                      "    return Bool[ismissing(x) for x in col]\n"
                      "end");
    if (NLSjl_exception_occurred()) {
        NLSjl_exception_clear();
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
