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
#include "dispBuiltin.hpp"
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
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"elementary_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "disp", (void*)Nelson::ElementaryFunctionsGateway::dispBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "size", (void*)Nelson::ElementaryFunctionsGateway::sizeBuiltin, -1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "length", (void*)Nelson::ElementaryFunctionsGateway::lengthBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "reshape", (void*)Nelson::ElementaryFunctionsGateway::reshapeBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "real", (void*)Nelson::ElementaryFunctionsGateway::realBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "imag", (void*)Nelson::ElementaryFunctionsGateway::imagBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "complex", (void*)Nelson::ElementaryFunctionsGateway::complexBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isequal", (void*)Nelson::ElementaryFunctionsGateway::isequalBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isequaln", (void*)Nelson::ElementaryFunctionsGateway::isequalnBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isequalto", (void*)Nelson::ElementaryFunctionsGateway::isequaltoBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "numel", (void*)Nelson::ElementaryFunctionsGateway::numelBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isapprox", (void*)Nelson::ElementaryFunctionsGateway::isapproxBuiltin, 1, -2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ceil", (void*)Nelson::ElementaryFunctionsGateway::ceilBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "floor", (void*)Nelson::ElementaryFunctionsGateway::floorBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fix", (void*)Nelson::ElementaryFunctionsGateway::fixBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "round", (void*)Nelson::ElementaryFunctionsGateway::roundBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isnan", (void*)Nelson::ElementaryFunctionsGateway::isnanBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isinf", (void*)Nelson::ElementaryFunctionsGateway::isinfBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isfinite", (void*)Nelson::ElementaryFunctionsGateway::isfiniteBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "ndims", (void*)Nelson::ElementaryFunctionsGateway::ndimsBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "conj", (void*)Nelson::ElementaryFunctionsGateway::conjBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "mod", (void*)Nelson::ElementaryFunctionsGateway::modBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "abs", (void*)Nelson::ElementaryFunctionsGateway::absBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "repmat", (void*)Nelson::ElementaryFunctionsGateway::repmatBuiltin, 1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "rem", (void*)Nelson::ElementaryFunctionsGateway::remBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "cast", (void*)Nelson::ElementaryFunctionsGateway::castBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "norm", (void*)Nelson::ElementaryFunctionsGateway::normBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "exp", (void*)Nelson::ElementaryFunctionsGateway::expBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "log", (void*)Nelson::ElementaryFunctionsGateway::logBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "log10", (void*)Nelson::ElementaryFunctionsGateway::log10Builtin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "log1p", (void*)Nelson::ElementaryFunctionsGateway::log1pBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sqrt", (void*)Nelson::ElementaryFunctionsGateway::sqrtBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "num2bin", (void*)Nelson::ElementaryFunctionsGateway::num2binBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "bin2num", (void*)Nelson::ElementaryFunctionsGateway::bin2numBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "swapbytes", (void*)Nelson::ElementaryFunctionsGateway::swapbytesBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "base2dec", (void*)Nelson::ElementaryFunctionsGateway::base2decBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "bin2dec", (void*)Nelson::ElementaryFunctionsGateway::bin2decBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "hex2dec", (void*)Nelson::ElementaryFunctionsGateway::hex2decBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dec2base", (void*)Nelson::ElementaryFunctionsGateway::dec2baseBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dec2bin", (void*)Nelson::ElementaryFunctionsGateway::dec2binBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "dec2hex", (void*)Nelson::ElementaryFunctionsGateway::dec2hexBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "linspace", (void*)Nelson::ElementaryFunctionsGateway::linspaceBuiltin, 1, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "log2", (void*)Nelson::ElementaryFunctionsGateway::log2Builtin, 2, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "fliplr", (void*)Nelson::ElementaryFunctionsGateway::fliplrBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "flipud", (void*)Nelson::ElementaryFunctionsGateway::flipudBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "find", (void*)Nelson::ElementaryFunctionsGateway::findBuiltin, 3, 3,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isvector", (void*)Nelson::ElementaryFunctionsGateway::isvectorBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "isscalar", (void*)Nelson::ElementaryFunctionsGateway::isscalarBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "triu", (void*)Nelson::ElementaryFunctionsGateway::triuBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "tril", (void*)Nelson::ElementaryFunctionsGateway::trilBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "sign", (void*)Nelson::ElementaryFunctionsGateway::signBuiltin, 1, 1,
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
