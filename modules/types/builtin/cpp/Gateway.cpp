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
#include "classBuiltin.hpp"
#include "isaBuiltin.hpp"
#include "iscellBuiltin.hpp"
#include "ischarBuiltin.hpp"
#include "isclassBuiltin.hpp"
#include "isdoubleBuiltin.hpp"
#include "isemptyBuiltin.hpp"
#include "isfloatBuiltin.hpp"
#include "ishandleBuiltin.hpp"
#include "isint16Builtin.hpp"
#include "isint32Builtin.hpp"
#include "isint64Builtin.hpp"
#include "isint8Builtin.hpp"
#include "isintegerBuiltin.hpp"
#include "islogicalBuiltin.hpp"
#include "isnumericBuiltin.hpp"
#include "isrealBuiltin.hpp"
#include "issingleBuiltin.hpp"
#include "issparseBuiltin.hpp"
#include "isstructBuiltin.hpp"
#include "isuint16Builtin.hpp"
#include "isuint32Builtin.hpp"
#include "isuint64Builtin.hpp"
#include "isuint8Builtin.hpp"
#include "isstringBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"types";
//=============================================================================
static const nlsGateway gateway[] = {
    { "class", Nelson::TypeGateway::classBuiltin, 1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "ischar", Nelson::TypeGateway::ischarBuiltin, 1, 1, CPP_BUILTIN },
    { "isstring", Nelson::TypeGateway::isstringBuiltin, 1, 1, CPP_BUILTIN },
    { "isfloat", Nelson::TypeGateway::isfloatBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "iscell", Nelson::TypeGateway::iscellBuiltin, 1, 1, CPP_BUILTIN },
    { "isdouble", Nelson::TypeGateway::isdoubleBuiltin, 1, 1, CPP_BUILTIN },
    { "issingle", Nelson::TypeGateway::issingleBuiltin, 1, 1, CPP_BUILTIN },
    { "isstruct", Nelson::TypeGateway::isstructBuiltin, 1, 1, CPP_BUILTIN },
    { "isinteger", Nelson::TypeGateway::isintegerBuiltin, 1, 1, CPP_BUILTIN },
    { "isint8", Nelson::TypeGateway::isint8Builtin, 1, 1, CPP_BUILTIN },
    { "isint16", Nelson::TypeGateway::isint16Builtin, 1, 1, CPP_BUILTIN },
    { "isint32", Nelson::TypeGateway::isint32Builtin, 1, 1, CPP_BUILTIN },
    { "isint64", Nelson::TypeGateway::isint64Builtin, 1, 1, CPP_BUILTIN },
    { "isuint8", Nelson::TypeGateway::isuint8Builtin, 1, 1, CPP_BUILTIN },
    { "isuint16", Nelson::TypeGateway::isuint16Builtin, 1, 1, CPP_BUILTIN },
    { "isuint32", Nelson::TypeGateway::isuint32Builtin, 1, 1, CPP_BUILTIN },
    { "isuint64", Nelson::TypeGateway::isuint64Builtin, 1, 1, CPP_BUILTIN },
    { "issparse", Nelson::TypeGateway::issparseBuiltin, 1, 1, CPP_BUILTIN },
    { "islogical", Nelson::TypeGateway::islogicalBuiltin, 1, 1, CPP_BUILTIN },
    { "isnumeric", Nelson::TypeGateway::isnumericBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isa", Nelson::TypeGateway::isaBuiltin, 1, 2, CPP_BUILTIN },
    { "isreal", Nelson::TypeGateway::isrealBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isempty", Nelson::TypeGateway::isemptyBuiltin, 1, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "isclass", Nelson::TypeGateway::isclassBuiltin, 1, 1, CPP_BUILTIN },
    { "ishandle", Nelson::TypeGateway::ishandleBuiltin, 1, 1, CPP_BUILTIN },
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
