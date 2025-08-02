//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JuliaRun.hpp"
#include "JuliaLibraryWrapper.hpp"
#include "JuliaEngine.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "JuliaTypesHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
JuliaRun(Interface* io, bool haveEventsLoop, const void* voidJuliaObjectHandle,
    const wstringVector& commands, const wstringVector& outputs, const wstringVector& names,
    const ArrayOfVector& values)
{
    if (!initializeJuliaEngine()) {
        Error(_W("Julia engine not initialized."));
    }

    jl_module_t* jl_main_module = (jl_module_t*)NLSjl_eval_string("Main");
    if (!jl_main_module) {
        Error(_W("Main module not found."));
    }

    ArrayOfVector retval;
    NLSjl_eval_string("clear_stdout()");
    NLSjl_eval_string("clear_stderr()");

    NLSjl_exception_clear();

    for (size_t i = 0; i < names.size(); ++i) {
        std::string name = wstring_to_utf8(names[i]);
        ArrayOf value = values[i];
        jl_value_t* x_value = ArrayOfTojl_value_t(value);
        if (!x_value) {
            Error(_W("Error converting value to Julia type: ") + names[i]);
        }
        std::string createVar = "global " + name;
        NLSjl_eval_string(createVar.c_str());
        NLSjl_set_global(jl_main_module, NLSjl_symbol(name.c_str()), x_value);
    }

    std::string lines;
    for (const auto& command : commands) {
        lines = lines + wstring_to_utf8(command) + "\n";
    }

    jl_value_t* result = NLSjl_eval_string(lines.c_str());
    if (!result) {
        jl_value_t* exception = NLSjl_exception_occurred();
        if (exception) {
            jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
            jl_function_t* sprint_func = NLSjl_get_function(jl_base_module, "sprint");
            jl_function_t* showerror_func = NLSjl_get_function(jl_base_module, "showerror");

            jl_value_t* error_string = NLSjl_call2(sprint_func, showerror_func, exception);
            std::string error_msg = NLSjl_string_ptr(error_string);

            jl_value_t* err = NLSjl_eval_string("get_stderr()");
            if (err) {
                std::string txt = std::string(NLSjl_string_ptr(err));
                if (!txt.empty()) {
                    error_msg += "\n" + txt;
                }
            }
            error_msg = _("Error in Julia: ") + "\n" + error_msg;
            Error(error_msg);
            return {};
        }
    }
    jl_value_t* output = NLSjl_eval_string("get_stdout()");
    if (output) {
        std::string txt = std::string(NLSjl_string_ptr(output));
        io->outputMessage(utf8_to_wstring(txt));
    }

    if (!outputs.empty()) {
        for (std::wstring variableName : outputs) {
            std::string name = wstring_to_utf8(variableName);
            jl_value_t* x_value = NLSjl_get_global(jl_main_module, NLSjl_symbol(name.c_str()));
            if (!x_value) {
                Error(_W("Variable not found: ") + variableName);
            }
            retval << jl_value_tToArrayOf(x_value);
        }
    }

    return retval;
}
//=============================================================================
}
//=============================================================================
