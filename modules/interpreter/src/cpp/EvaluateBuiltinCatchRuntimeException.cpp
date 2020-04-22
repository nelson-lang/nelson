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
#if _MSC_VER
#include <Windows.h>
#include <eh.h>
#else
#include <csetjmp>
#include <csignal>
#endif
#include "Error.hpp"
#include "EvaluateBuiltinCatchRuntimeException.hpp"
#include "i18n.hpp"
#include "NelsonGateway.hpp"
#include "CallMexBuiltin.hpp"
//=============================================================================
#ifndef _MSC_VER
static std::jmp_buf buf;
static int error_code = 0;
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
using BuiltInWithEvaluatorFuncPtr = ArrayOfVector (*)(Evaluator*, int, const ArrayOfVector&);
using BuiltInFuncPtr = ArrayOfVector (*)(int, const ArrayOfVector&);
//=============================================================================
#ifdef _MSC_VER
class InfoFromSE
{
public:
    using exception_code_t = unsigned int;
    //=============================================================================
    static const char*
    seDescription(const exception_code_t& code)
    {
        switch (code) {
        case EXCEPTION_ACCESS_VIOLATION:
            return "EXCEPTION_ACCESS_VIOLATION";
        case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
            return "EXCEPTION_ARRAY_BOUNDS_EXCEEDED";
        case EXCEPTION_BREAKPOINT:
            return "EXCEPTION_BREAKPOINT";
        case EXCEPTION_DATATYPE_MISALIGNMENT:
            return "EXCEPTION_DATATYPE_MISALIGNMENT";
        case EXCEPTION_FLT_DENORMAL_OPERAND:
            return "EXCEPTION_FLT_DENORMAL_OPERAND";
        case EXCEPTION_FLT_DIVIDE_BY_ZERO:
            return "EXCEPTION_FLT_DIVIDE_BY_ZERO";
        case EXCEPTION_FLT_INEXACT_RESULT:
            return "EXCEPTION_FLT_INEXACT_RESULT";
        case EXCEPTION_FLT_INVALID_OPERATION:
            return "EXCEPTION_FLT_INVALID_OPERATION";
        case EXCEPTION_FLT_OVERFLOW:
            return "EXCEPTION_FLT_OVERFLOW";
        case EXCEPTION_FLT_STACK_CHECK:
            return "EXCEPTION_FLT_STACK_CHECK";
        case EXCEPTION_FLT_UNDERFLOW:
            return "EXCEPTION_FLT_UNDERFLOW";
        case EXCEPTION_ILLEGAL_INSTRUCTION:
            return "EXCEPTION_ILLEGAL_INSTRUCTION";
        case EXCEPTION_IN_PAGE_ERROR:
            return "EXCEPTION_IN_PAGE_ERROR";
        case EXCEPTION_INT_DIVIDE_BY_ZERO:
            return "EXCEPTION_INT_DIVIDE_BY_ZERO";
        case EXCEPTION_INT_OVERFLOW:
            return "EXCEPTION_INT_OVERFLOW";
        case EXCEPTION_INVALID_DISPOSITION:
            return "EXCEPTION_INVALID_DISPOSITION";
        case EXCEPTION_NONCONTINUABLE_EXCEPTION:
            return "EXCEPTION_NONCONTINUABLE_EXCEPTION";
        case EXCEPTION_PRIV_INSTRUCTION:
            return "EXCEPTION_PRIV_INSTRUCTION";
        case EXCEPTION_SINGLE_STEP:
            return "EXCEPTION_SINGLE_STEP";
        case EXCEPTION_STACK_OVERFLOW:
            return "EXCEPTION_STACK_OVERFLOW";
        default:
            return "UNKNOWN EXCEPTION";
        }
    }
    //=============================================================================
    static std::string
    information(struct _EXCEPTION_POINTERS* ep, exception_code_t code = 0)
    {
        std::string errorSystem = _("System error detected: ") + std::string(seDescription(code));
        return errorSystem;
    }
    //=============================================================================
};
//=============================================================================
void
translator_SE(InfoFromSE::exception_code_t code, struct _EXCEPTION_POINTERS* ep)
{
    throw std::runtime_error(InfoFromSE::information(ep, code));
}
//=============================================================================
#else
static void
signal_handler(int signal_code)
{
    error_code = signal_code;
    std::longjmp(buf, 1);
}
#endif
//=============================================================================
#ifdef _MSC_VER
ArrayOfVector
EvaluateBuiltinCatchRuntimeException(
    Evaluator* eval, void* fptr, ArrayOfVector& inputs, int nargout, size_t builtinPrototype)
{
    ArrayOfVector outputs;
    switch (builtinPrototype) {
    case BUILTIN_PROTOTYPE::CPP_BUILTIN: {
        BuiltInFuncPtr builtinPtr = (BuiltInFuncPtr)fptr;
        _set_se_translator(translator_SE);
        try {
            outputs = builtinPtr(nargout, inputs);
        } catch (const std::runtime_error& e) {
            _set_se_translator(nullptr);
            Error(e.what());
        }
        _set_se_translator(nullptr);

    } break;
    case BUILTIN_PROTOTYPE::CPP_BUILTIN_WITH_EVALUATOR: {
        BuiltInWithEvaluatorFuncPtr builtinPtr = (BuiltInWithEvaluatorFuncPtr)fptr;
        _set_se_translator(translator_SE);
        try {
            outputs = builtinPtr(eval, nargout, inputs);
        } catch (const std::runtime_error& e) {
            _set_se_translator(nullptr);
            Error(e.what());
        }
        _set_se_translator(nullptr);

    } break;
    case BUILTIN_PROTOTYPE::C_MEX_BUILTIN: {
        CallMexBuiltin(fptr, inputs, nargout, outputs);
    } break;
    default: {
        Error(_("BUILTIN type not managed."));
    } break;
    }
    return outputs;
}
#endif
//=============================================================================
#ifndef _MSC_VER
ArrayOfVector
EvaluateBuiltinCatchRuntimeException(
    Evaluator* eval, void* fptr, ArrayOfVector& inputs, int nargout, size_t builtinPrototype)
{
    ArrayOfVector outputs;
    error_code = 0;
    signal(SIGSEGV, signal_handler);
    signal(SIGFPE, signal_handler);
    signal(SIGILL, signal_handler);
    if (!(setjmp(buf))) {
        switch (builtinPrototype) {
        case BUILTIN_PROTOTYPE::CPP_BUILTIN: {
            BuiltInFuncPtr builtinPtr = (BuiltInFuncPtr)fptr;
            outputs = builtinPtr(nargout, inputs);
        } break;
        case BUILTIN_PROTOTYPE::CPP_BUILTIN_WITH_EVALUATOR: {
            BuiltInWithEvaluatorFuncPtr builtinPtr = (BuiltInWithEvaluatorFuncPtr)fptr;
            outputs = builtinPtr(eval, nargout, inputs);
        } break;
        case BUILTIN_PROTOTYPE::C_MEX_BUILTIN: {
            CallMexBuiltin(fptr, inputs, nargout, outputs);
        } break;
        default: { } break; }
    } else {
        std::string error_message = "";
        switch (error_code) {
        case SIGSEGV: {
            error_message = _("System error detected: ") + std::string("SIGSEV");
        } break;
        case SIGFPE: {
            error_message = _("System error detected: ") + std::string("SIGFPE");
        } break;
        case SIGILL: {
            error_message = _("System error detected: ") + std::string("SIGILL");
        } break;
        default: {
            error_message
                = _("System error detected.") + " " + _("Error code:") + std::to_string(error_code);
        } break;
        }
        signal(SIGSEGV, SIG_DFL);
        signal(SIGFPE, SIG_DFL);
        signal(SIGILL, SIG_DFL);
        Error(error_message);
    }
    signal(SIGSEGV, SIG_DFL);
    signal(SIGFPE, SIG_DFL);
    signal(SIGILL, SIG_DFL);
    return outputs;
}
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
