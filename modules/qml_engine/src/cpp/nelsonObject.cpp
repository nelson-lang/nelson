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
#include "nelsonObject.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "Evaluator.hpp"
#include "Interface.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "ProcessEvents.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static Evaluator *eval = nullptr;
//=============================================================================
nelsonObject::nelsonObject(QObject *parent) :
    QObject(parent)
{
    if (eval == nullptr)
    {
        eval = (Evaluator *)GetNelsonMainEvaluatorDynamicFunction();
    }
}
//=============================================================================
void nelsonObject::disp(QString msg)
{
    if (eval)
    {
        Interface *io = eval->getInterface();
        if (io)
        {
            std::wstring wstr = L"disp('" + QStringTowstring(msg) + L"');\n";
            std::string ustr = wstring_to_utf8(wstr);
            eval->commandQueue.add(ustr);
        }
    }
}
//=============================================================================
void nelsonObject::evaluate(QString msg)
{
    if (eval)
    {
        std::wstring wstr = QStringTowstring(msg);
        std::string ustr = wstring_to_utf8(wstr);
        Interface *io = eval->getInterface();
        if (io)
        {
            /*
            			if (io->isAtPrompt())
            			{
            				eval->commandQueue.add(ustr + "\n");
            			}
            			else
            			{
            				eval->evaluateString(ustr + "\n");
            			}
            */
        }
    }
}
//=============================================================================
void nelsonObject::processevent()
{
    ProcessEvents();
}
//=============================================================================
