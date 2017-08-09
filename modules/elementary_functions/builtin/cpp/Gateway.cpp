//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "plusBuiltin.hpp"
#include "andBuiltin.hpp"
#include "colonBuiltin.hpp"
#include "ctransposeBuiltin.hpp"
#include "dispBuiltin.hpp"
#include "eqBuiltin.hpp"
#include "geBuiltin.hpp"
#include "gtBuiltin.hpp"
#include "ldivideBuiltin.hpp"
#include "mldivideBuiltin.hpp"
#include "mrdivideBuiltin.hpp"
#include "rdivideBuiltin.hpp"
#include "leBuiltin.hpp"
#include "ltBuiltin.hpp"
#include "minusBuiltin.hpp"
#include "mpowerBuiltin.hpp"
#include "mtimesBuiltin.hpp"
#include "neBuiltin.hpp"
#include "notBuiltin.hpp"
#include "orBuiltin.hpp"
#include "powerBuiltin.hpp"
#include "shortcutorBuiltin.hpp"
#include "shortcutandBuiltin.hpp"
#include "subsindexBuiltin.hpp"
#include "timesBuiltin.hpp"
#include "transposeBuiltin.hpp"
#include "uminusBuiltin.hpp"
#include "sizeBuiltin.hpp"
#include "lengthBuiltin.hpp"
#include "reshapeBuiltin.hpp"
#include "complexBuiltin.hpp"
#include "imagBuiltin.hpp"
#include "realBuiltin.hpp"
#include "horzcatBuiltin.hpp"
#include "vertcatBuiltin.hpp"
#include "isequalBuiltin.hpp"
#include "isequalnBuiltin.hpp"
#include "numelBuiltin.hpp"
#include "isapproxBuiltin.hpp"
#include "uplusBuiltin.hpp"
#include "ceilBuiltin.hpp"
#include "floorBuiltin.hpp"
#include "roundBuiltin.hpp"
#include "fixBuiltin.hpp"
#include "isnanBuiltin.hpp"
#include "isinfBuiltin.hpp"
#include "isfiniteBuiltin.hpp"
#include "ndimsBuiltin.hpp"
#include "conjBuiltin.hpp"
#include "prodBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"elementary_functions";
//=============================================================================
static const nlsGateway gateway[] =
{
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
    { "power", Nelson::ElementaryFunctionsGateway::powerBuiltin, 1, 2 },
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
    { "numel", Nelson::ElementaryFunctionsGateway::numelBuiltin, 1, 1 },
    { "isapprox", Nelson::ElementaryFunctionsGateway::isapproxBuiltin, 1, -2},
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
