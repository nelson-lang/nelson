//=============================================================================
// Copyright (c) 2018 Allan CORNET (Nelson)
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer. Redistributions in binary
// form must reproduce the above copyright notice, this list of conditions and
// the following disclaimer in the documentation and/or other materials
// provided with the distribution. Neither the name of the nor the names of
// its contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//=============================================================================
#include "cpp_sumBuiltin.hpp"
#include "cpp_sum.hpp"
#include "Error.hpp"
//=============================================================================
ArrayOfVector cpp_sumBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2)
    {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    retval.push_back(
        ArrayOf::doubleConstructor(
            cpp_sum(param1.getContentAsDoubleScalar(), param2.getContentAsDoubleScalar())));
    return retval;
}
//=============================================================================
