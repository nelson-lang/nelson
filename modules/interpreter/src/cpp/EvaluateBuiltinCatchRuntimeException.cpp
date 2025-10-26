//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#define WIN32_LEAN_AND_MEAN
#pragma warning(disable : 4535)
#include <Windows.h>
#include <eh.h>
#else
#include <csetjmp>
#include <csignal>
#endif
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
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
EvaluateBuiltinCatchRuntimeException(Evaluator* eval, void* fptr, const ArrayOfVector& inputs,
    int nargout, size_t builtinPrototype, bool interleavedComplex)
{
    ArrayOfVector outputs;
    switch (builtinPrototype) {
    case BUILTIN_PROTOTYPE::CPP_BUILTIN: {
        auto builtinPtr = (BuiltInFuncPtr)fptr;
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
        auto builtinPtr = (BuiltInWithEvaluatorFuncPtr)fptr;
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
        CallMexBuiltin(fptr, inputs, nargout, outputs, interleavedComplex);
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
EvaluateBuiltinCatchRuntimeException(Evaluator* eval, void* fptr, const ArrayOfVector& inputs,
    int nargout, size_t builtinPrototype, bool interleavedComplex)
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
            CallMexBuiltin(fptr, inputs, nargout, outputs, interleavedComplex);
        } break;
        default: {
        } break;
        }
    } else {
        std::string error_message = "";
        switch (error_code) {
        case SIGSEGV: {
            error_message = fmt::sprintf(_("System error detected: %s"), "SIGSEGV");
        } break;
        case SIGFPE: {
            error_message = fmt::sprintf(_("System error detected: %s"), "SIGFPE");
        } break;
        case SIGILL: {
            error_message = fmt::sprintf(_("System error detected: %s"), "SIGILL");
        } break;
        default: {
            error_message = fmt::sprintf(
                _("System error detected: Error code: %s"), std::to_string(error_code));
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
