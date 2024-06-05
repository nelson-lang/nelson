//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelsonGateway.hpp"
#include "absBuiltin.hpp"
#include "castBuiltin.hpp"
#include "ceilBuiltin.hpp"
#include "complexBuiltin.hpp"
#include "conjBuiltin.hpp"
#include "fixBuiltin.hpp"
#include "floorBuiltin.hpp"
#include "imagBuiltin.hpp"
#include "isapproxBuiltin.hpp"
#include "isequalBuiltin.hpp"
#include "isequalnBuiltin.hpp"
#include "isequaltoBuiltin.hpp"
#include "isfiniteBuiltin.hpp"
#include "isinfBuiltin.hpp"
#include "isnanBuiltin.hpp"
#include "lengthBuiltin.hpp"
#include "modBuiltin.hpp"
#include "ndimsBuiltin.hpp"
#include "numelBuiltin.hpp"
#include "realBuiltin.hpp"
#include "remBuiltin.hpp"
#include "repmatBuiltin.hpp"
#include "reshapeBuiltin.hpp"
#include "roundBuiltin.hpp"
#include "sizeBuiltin.hpp"
#include "normBuiltin.hpp"
#include "expBuiltin.hpp"
#include "logBuiltin.hpp"
#include "log10Builtin.hpp"
#include "log1pBuiltin.hpp"
#include "sqrtBuiltin.hpp"
#include "num2binBuiltin.hpp"
#include "bin2numBuiltin.hpp"
#include "swapbytesBuiltin.hpp"
#include "base2decBuiltin.hpp"
#include "dec2baseBuiltin.hpp"
#include "linspaceBuiltin.hpp"
#include "log2Builtin.hpp"
#include "fliplrBuiltin.hpp"
#include "flipudBuiltin.hpp"
#include "findBuiltin.hpp"
#include "isvectorBuiltin.hpp"
#include "isscalarBuiltin.hpp"
#include "triuBuiltin.hpp"
#include "trilBuiltin.hpp"
#include "signBuiltin.hpp"
#include "hypotBuiltin.hpp"
#include "permuteBuiltin.hpp"
#include "__rot90_matrix_2D__Builtin.hpp"
#include "allfiniteBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"elementary_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "flipud", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::flipudBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fliplr", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::fliplrBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cast", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::castBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "hypot", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::hypotBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "isequal", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isequalBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "isequaln", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isequalnBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "isequalto", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isequaltoBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { "size", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::sizeBuiltin, -1, 2 },
    { "length", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::lengthBuiltin, 1, 1 },
    { "reshape", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::reshapeBuiltin, 1, -1 },
    { "real", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::realBuiltin, 1, 1 },
    { "imag", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::imagBuiltin, 1, -1 },
    { "complex", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::complexBuiltin, 1, 2 },
    { "numel", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::numelBuiltin, 1, 1 },
    { "isapprox", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isapproxBuiltin, 1, -2 },
    { "ceil", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::ceilBuiltin, 1, 1 },
    { "floor", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::floorBuiltin, 1, 1 },
    { "fix", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::fixBuiltin, 1, 1 },
    { "round", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::roundBuiltin, 1, 1 },
    { "isnan", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isnanBuiltin, 1, 1 },
    { "isinf", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isinfBuiltin, 1, 1 },
    { "isfinite", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isfiniteBuiltin, 1, 1 },
    { "ndims", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::ndimsBuiltin, 1, 1 },
    { "conj", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::conjBuiltin, 1, 1 },
    { "mod", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::modBuiltin, 1, 2 },
    { "abs", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::absBuiltin, 1, 1 },
    { "repmat", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::repmatBuiltin, 1, -1 },
    { "rem", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::remBuiltin, 1, 2 },
    { "norm", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::normBuiltin, 1, 1 },
    { "exp", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::expBuiltin, 1, 1 },
    { "log", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::logBuiltin, 1, 1 },
    { "log10", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::log10Builtin, 1, 1 },
    { "log1p", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::log1pBuiltin, 1, 1 },
    { "sqrt", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::sqrtBuiltin, 1, 1 },
    { "num2bin", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::num2binBuiltin, 1, 1 },
    { "bin2num", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::bin2numBuiltin, 1, 1 },
    { "swapbytes", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::swapbytesBuiltin, 1, 1 },
    { "base2dec", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::base2decBuiltin, 1, 2 },
    { "bin2dec", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::bin2decBuiltin, 1, 1 },
    { "hex2dec", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::hex2decBuiltin, 1, 1 },
    { "dec2base", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::dec2baseBuiltin, 1, 3 },
    { "dec2bin", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::dec2binBuiltin, 1, 2 },
    { "dec2hex", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::dec2hexBuiltin, 1, 2 },
    { "linspace", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::linspaceBuiltin, 1, 3 },
    { "log2", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::log2Builtin, 2, 1 },
    { "find", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::findBuiltin, 3, 3 },
    { "isvector", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isvectorBuiltin, 1, 1 },
    { "isscalar", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isscalarBuiltin, 1, 1 },
    { "triu", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::triuBuiltin, 1, 2 },
    { "tril", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::trilBuiltin, 1, 2 },
    { "sign", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::signBuiltin, 1, 1 },
    { "permute", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::permuteBuiltin, 1, 2 },
    { "__rot90_matrix_2D__",
        (ptrBuiltin)Nelson::ElementaryFunctionsGateway::__rot90_matrix_2D__Builtin, 1, 2 },
    { "allfinite", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::allfiniteBuiltin, 1, 1 },
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
NLSGATEWAYREMOVE(gateway)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
