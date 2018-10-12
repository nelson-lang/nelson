//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#include "absBuiltin.hpp"
#include "andBuiltin.hpp"
#include "castBuiltin.hpp"
#include "ceilBuiltin.hpp"
#include "colonBuiltin.hpp"
#include "complexBuiltin.hpp"
#include "conjBuiltin.hpp"
#include "ctransposeBuiltin.hpp"
#include "dispBuiltin.hpp"
#include "eqBuiltin.hpp"
#include "fixBuiltin.hpp"
#include "floorBuiltin.hpp"
#include "geBuiltin.hpp"
#include "gtBuiltin.hpp"
#include "horzcatBuiltin.hpp"
#include "imagBuiltin.hpp"
#include "isapproxBuiltin.hpp"
#include "isequalBuiltin.hpp"
#include "isequalnBuiltin.hpp"
#include "isequaltoBuiltin.hpp"
#include "isfiniteBuiltin.hpp"
#include "isinfBuiltin.hpp"
#include "isnanBuiltin.hpp"
#include "ldivideBuiltin.hpp"
#include "leBuiltin.hpp"
#include "lengthBuiltin.hpp"
#include "ltBuiltin.hpp"
#include "minusBuiltin.hpp"
#include "mldivideBuiltin.hpp"
#include "modBuiltin.hpp"
#include "mpowerBuiltin.hpp"
#include "mrdivideBuiltin.hpp"
#include "mtimesBuiltin.hpp"
#include "ndimsBuiltin.hpp"
#include "neBuiltin.hpp"
#include "notBuiltin.hpp"
#include "allBuiltin.hpp"
#include "anyBuiltin.hpp"
#include "numelBuiltin.hpp"
#include "orBuiltin.hpp"
#include "plusBuiltin.hpp"
#include "powerBuiltin.hpp"
#include "prodBuiltin.hpp"
#include "rdivideBuiltin.hpp"
#include "realBuiltin.hpp"
#include "remBuiltin.hpp"
#include "repmatBuiltin.hpp"
#include "reshapeBuiltin.hpp"
#include "roundBuiltin.hpp"
#include "shortcutandBuiltin.hpp"
#include "shortcutorBuiltin.hpp"
#include "sizeBuiltin.hpp"
#include "subsindexBuiltin.hpp"
#include "timesBuiltin.hpp"
#include "transposeBuiltin.hpp"
#include "uminusBuiltin.hpp"
#include "uplusBuiltin.hpp"
#include "vertcatBuiltin.hpp"
#include "normBuiltin.hpp"
#include "expBuiltin.hpp"
#include "logBuiltin.hpp"
#include "sqrtBuiltin.hpp"
#include "ismissingBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"elementary_functions";
//=============================================================================
static const nlsGateway gateway[] = {
    { "plus", Nelson::ElementaryFunctionsGateway::plusBuiltin, 1, 2 },
    { "minus", Nelson::ElementaryFunctionsGateway::minusBuiltin, 1, 2 },
    { "uminus", Nelson::ElementaryFunctionsGateway::uminusBuiltin, 1, 1 },
    { "uplus", Nelson::ElementaryFunctionsGateway::uplusBuiltin, 1, 1 },
    { "and", Nelson::ElementaryFunctionsGateway::andBuiltin, 1, 2 },
    { "colon", Nelson::ElementaryFunctionsGateway::colonBuiltin, 1, 2 },
    { "ctranspose", Nelson::ElementaryFunctionsGateway::ctransposeBuiltin, 1, 1 },
    { "transpose", Nelson::ElementaryFunctionsGateway::transposeBuiltin, 1, 1 },
    { "disp", Nelson::ElementaryFunctionsGateway::dispBuiltin, 0, 1 },
    { "eq", Nelson::ElementaryFunctionsGateway::eqBuiltin, 1, 2 },
    { "ge", Nelson::ElementaryFunctionsGateway::geBuiltin, 1, 2 },
    { "gt", Nelson::ElementaryFunctionsGateway::gtBuiltin, 1, 2 },
    { "le", Nelson::ElementaryFunctionsGateway::leBuiltin, 1, 2 },
    { "lt", Nelson::ElementaryFunctionsGateway::ltBuiltin, 1, 2 },
    { "ne", Nelson::ElementaryFunctionsGateway::neBuiltin, 1, 2 },
    { "ldivide", Nelson::ElementaryFunctionsGateway::ldivideBuiltin, 1, 2 },
    { "rdivide", Nelson::ElementaryFunctionsGateway::rdivideBuiltin, 1, 2 },
    { "mldivide", Nelson::ElementaryFunctionsGateway::mldivideBuiltin, 1, 2 },
    { "mrdivide", Nelson::ElementaryFunctionsGateway::mrdivideBuiltin, 1, 2 },
    { "mtimes", Nelson::ElementaryFunctionsGateway::mtimesBuiltin, 1, 2 },
    { "times", Nelson::ElementaryFunctionsGateway::timesBuiltin, 1, 2 },
    { "not", Nelson::ElementaryFunctionsGateway::notBuiltin, 1, 1 },
    { "or", Nelson::ElementaryFunctionsGateway::orBuiltin, 1, 2 },
    { "any", Nelson::ElementaryFunctionsGateway::anyBuiltin, 1, 2 },
    { "all", Nelson::ElementaryFunctionsGateway::allBuiltin, 1, 1 },
    { "power", Nelson::ElementaryFunctionsGateway::powerBuiltin, 1, 2 },
    { "mpower", Nelson::ElementaryFunctionsGateway::mpowerBuiltin, 1, 2 },
    { "shortcutor", Nelson::ElementaryFunctionsGateway::shortcutorBuiltin, 1, 2 },
    { "shortcutand", Nelson::ElementaryFunctionsGateway::shortcutandBuiltin, 1, 2 },
    { "subsindex", Nelson::ElementaryFunctionsGateway::subsindexBuiltin, 1, 1 },
    //"subsasgn"
    //"subsref"
    { "size", Nelson::ElementaryFunctionsGateway::sizeBuiltin, -1, 2 },
    { "length", Nelson::ElementaryFunctionsGateway::lengthBuiltin, 1, 1 },
    { "reshape", Nelson::ElementaryFunctionsGateway::reshapeBuiltin, 1, -1 },
    { "real", Nelson::ElementaryFunctionsGateway::realBuiltin, 1, 1 },
    { "imag", Nelson::ElementaryFunctionsGateway::imagBuiltin, 1, -1 },
    { "complex", Nelson::ElementaryFunctionsGateway::complexBuiltin, 1, 2 },
    { "horzcat", Nelson::ElementaryFunctionsGateway::horzcatBuiltin, 1, 1 },
    { "vertcat", Nelson::ElementaryFunctionsGateway::vertcatBuiltin, 1, 1 },
    { "isequal", Nelson::ElementaryFunctionsGateway::isequalBuiltin, 1, -1 },
    { "isequaln", Nelson::ElementaryFunctionsGateway::isequalnBuiltin, 1, -1 },
    { "isequalto", Nelson::ElementaryFunctionsGateway::isequaltoBuiltin, 1, -1 },
    { "numel", Nelson::ElementaryFunctionsGateway::numelBuiltin, 1, 1 },
    { "isapprox", Nelson::ElementaryFunctionsGateway::isapproxBuiltin, 1, -2 },
    { "ceil", Nelson::ElementaryFunctionsGateway::ceilBuiltin, 1, 1 },
    { "floor", Nelson::ElementaryFunctionsGateway::floorBuiltin, 1, 1 },
    { "fix", Nelson::ElementaryFunctionsGateway::fixBuiltin, 1, 1 },
    { "round", Nelson::ElementaryFunctionsGateway::roundBuiltin, 1, 1 },
    { "isnan", Nelson::ElementaryFunctionsGateway::isnanBuiltin, 1, 1 },
    { "isinf", Nelson::ElementaryFunctionsGateway::isinfBuiltin, 1, 1 },
    { "isfinite", Nelson::ElementaryFunctionsGateway::isfiniteBuiltin, 1, 1 },
    { "ndims", Nelson::ElementaryFunctionsGateway::ndimsBuiltin, 1, 1 },
    { "conj", Nelson::ElementaryFunctionsGateway::conjBuiltin, 1, 1 },
    { "prod", Nelson::ElementaryFunctionsGateway::prodBuiltin, 1, 3 },
    { "mod", Nelson::ElementaryFunctionsGateway::modBuiltin, 1, 2 },
    { "abs", Nelson::ElementaryFunctionsGateway::absBuiltin, 1, 1 },
    { "repmat", Nelson::ElementaryFunctionsGateway::repmatBuiltin, 1, -1 },
    { "rem", Nelson::ElementaryFunctionsGateway::remBuiltin, 1, 2 },
    { "cast", Nelson::ElementaryFunctionsGateway::castBuiltin, 1, 3 },
    { "norm", Nelson::ElementaryFunctionsGateway::normBuiltin, 1, 1 },
    { "exp", Nelson::ElementaryFunctionsGateway::expBuiltin, 1, 1 },
    { "log", Nelson::ElementaryFunctionsGateway::logBuiltin, 1, 1 },
    { "sqrt", Nelson::ElementaryFunctionsGateway::sqrtBuiltin, 1, 1 },
    { "ismissing", Nelson::ElementaryFunctionsGateway::ismissingBuiltin, 1, 1 },
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
