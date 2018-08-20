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
#include "TextEditor.hpp"
#include "editorBuiltin.hpp"
#include "smartindentBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"text_editor";
//=============================================================================
static const nlsGateway gateway[] = {
    { "editor", Nelson::TextEditorGateway::editorBuiltin, 0, 0, CPP_BUILTIN },
    { "smartindent", Nelson::TextEditorGateway::smartindentBuiltin, 0, -1, CPP_BUILTIN },
};
//=============================================================================
NLSGATEWAYFUNC(gateway)
//=============================================================================
NLSGATEWAYINFO(gateway)
//=============================================================================
static bool
finishTextEditorModule(Nelson::Evaluator* eval)
{
    closeEditor();
    return true;
}
//=============================================================================
NLSGATEWAYREMOVEEXTENDED(gateway, (void*)finishTextEditorModule)
//=============================================================================
NLSGATEWAYNAME()
//=============================================================================
