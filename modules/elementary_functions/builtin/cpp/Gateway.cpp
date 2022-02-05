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
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"elementary_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "size", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::sizeBuiltin, -1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "length", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::lengthBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "reshape", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::reshapeBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "real", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::realBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "imag", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::imagBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "complex", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::complexBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isequal", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isequalBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isequaln", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isequalnBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isequalto", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isequaltoBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "numel", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::numelBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isapprox", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isapproxBuiltin, 1, -2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ceil", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::ceilBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "floor", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::floorBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fix", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::fixBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "round", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::roundBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isnan", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isnanBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isinf", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isinfBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isfinite", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isfiniteBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ndims", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::ndimsBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "conj", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::conjBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mod", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::modBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "abs", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::absBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "repmat", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::repmatBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rem", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::remBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cast", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::castBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "norm", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::normBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "exp", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::expBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "log", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::logBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "log10", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::log10Builtin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "log1p", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::log1pBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sqrt", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::sqrtBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "num2bin", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::num2binBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "bin2num", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::bin2numBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "swapbytes", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::swapbytesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "base2dec", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::base2decBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "bin2dec", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::bin2decBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "hex2dec", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::hex2decBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dec2base", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::dec2baseBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dec2bin", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::dec2binBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dec2hex", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::dec2hexBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "linspace", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::linspaceBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "log2", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::log2Builtin, 2, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fliplr", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::fliplrBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "flipud", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::flipudBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "find", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::findBuiltin, 3, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isvector", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isvectorBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isscalar", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::isscalarBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "triu", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::triuBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "tril", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::trilBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sign", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::signBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "hypot", (ptrBuiltin)Nelson::ElementaryFunctionsGateway::hypotBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },

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
