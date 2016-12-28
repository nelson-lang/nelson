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
#include "classBuiltin.hpp"
#include "ischarBuiltin.hpp"
#include "iscellBuiltin.hpp"
#include "isfloatBuiltin.hpp"
#include "isdoubleBuiltin.hpp"
#include "issingleBuiltin.hpp"
#include "isstructBuiltin.hpp"
#include "isintegerBuiltin.hpp"
#include "isint8Builtin.hpp"
#include "isint16Builtin.hpp"
#include "isint32Builtin.hpp"
#include "isint64Builtin.hpp"
#include "isuint8Builtin.hpp"
#include "isuint16Builtin.hpp"
#include "isuint32Builtin.hpp"
#include "isuint64Builtin.hpp"
#include "issparseBuiltin.hpp"
#include "islogicalBuiltin.hpp"
#include "isnumericBuiltin.hpp"
#include "isaBuiltin.hpp"
#include "isrealBuiltin.hpp"
#include "isemptyBuiltin.hpp"
#include "isclassBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"types";
//=============================================================================
static const nlsGateway gateway[] =
{
    { "class", Nelson::TypeGateway::classBuiltin, 1, 1 },
    { "ischar", Nelson::TypeGateway::ischarBuiltin, 1, 1 },
    { "isfloat", Nelson::TypeGateway::isfloatBuiltin, 1, 1 },
    { "iscell", Nelson::TypeGateway::iscellBuiltin, 1, 1 },
    { "isdouble", Nelson::TypeGateway::isdoubleBuiltin, 1, 1 },
    { "issingle", Nelson::TypeGateway::issingleBuiltin, 1, 1 },
    { "isstruct", Nelson::TypeGateway::isstructBuiltin, 1, 1 },
    { "isinteger", Nelson::TypeGateway::isintegerBuiltin, 1, 1 },
    { "isint8", Nelson::TypeGateway::isint8Builtin, 1, 1 },
    { "isint16", Nelson::TypeGateway::isint16Builtin, 1, 1 },
    { "isint32", Nelson::TypeGateway::isint32Builtin, 1, 1 },
    { "isint64", Nelson::TypeGateway::isint64Builtin, 1, 1 },
    { "isuint8", Nelson::TypeGateway::isuint8Builtin, 1, 1 },
    { "isuint16", Nelson::TypeGateway::isuint16Builtin, 1, 1 },
    { "isuint32", Nelson::TypeGateway::isuint32Builtin, 1, 1 },
    { "isuint64", Nelson::TypeGateway::isuint64Builtin, 1, 1 },
    { "issparse", Nelson::TypeGateway::issparseBuiltin, 1, 1 },
    { "islogical", Nelson::TypeGateway::islogicalBuiltin, 1, 1 },
    { "isnumeric", Nelson::TypeGateway::isnumericBuiltin, 1, 1 },
    { "isa", Nelson::TypeGateway::isaBuiltin, 1, 2 },
    { "isreal", Nelson::TypeGateway::isrealBuiltin, 1, 1 },
    { "isempty", Nelson::TypeGateway::isemptyBuiltin, 1, 1 },
    { "isclass", Nelson::TypeGateway::isclassBuiltin, 1, 1 },

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
