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
#include <QtQml/QQmlComponent>
#include <QtCore/qrect.h>
#include "DispQmlHandleObject.hpp"
#include "HandleManager.hpp"
#include "QmlHandleObject.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "fieldnamesQmlHandleObject.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	static void DispQmlHandleObject(Interface *io, QmlHandleObject *qmlHandle)
	{
		if (qmlHandle != nullptr)
		{
			wstringVector wfieldnames;
			fieldnamesQmlHandleObject(qmlHandle, false, wfieldnames);
			QObject *qobj = (QObject *)qmlHandle->getPointer();
			if (qobj)
			{
				std::wstring msg;
				std::wstring line;

				QObject *parent = qobj->parent();
				if (parent)
				{
					line = L"\t" + std::wstring(L"parent") + L": " + L"handle" + L"\n";
				}
				else
				{
					line = L"\t" + std::wstring(L"parent") + L": " + L"handle  []" + L"\n";
				}
				msg = msg + line;

				QObjectList childs = qobj->children();
				int s = childs.size();
				if (s > 0)
				{
					line = L"\t" + std::wstring(L"children") + L": " + L"handle 1x" + std::to_wstring(s) + L"\n";
				}
				else
				{
					line = L"\t" + std::wstring(L"children") + L": " + L"handle []" + L"\n";
				}
				msg = msg + line;

				for (size_t k = 0; k < wfieldnames.size(); k++)
				{
					if (wfieldnames[k] == L"parent")
					{
					}
					else if (wfieldnames[k] == L"children")
					{
					}
					else
					{
						QVariant propertyValue = qobj->property(wstring_to_utf8(wfieldnames[k]).c_str());
						if (propertyValue.isValid())
						{
							switch (propertyValue.type())
							{
							case QMetaType::QRect:
							{
								QRect qrect = propertyValue.toRect();
								int x = qrect.x();
								int y = qrect.y();
								int w = qrect.width();
								int h = qrect.height();
								std::wstring wstr = L"x:" + std::to_wstring(x) + L" " +
									L"y:" + std::to_wstring(y) + L" " +
									L"w:" + std::to_wstring(w) + L" " +
									L"h:" + std::to_wstring(h);
								line = L"\t" + wfieldnames[k] + L": " + wstr + L"\n";
							}
							break;
							case QMetaType::QRectF:
							{
								QRectF qrect = propertyValue.toRectF();
								double x = qrect.x();
								double y = qrect.y();
								double w = qrect.width();
								double h = qrect.height();
								double intpart;
								std::wstring wstr;
								if (std::modf(x, &intpart) == 0.0 &&
									std::modf(y, &intpart) == 0.0 &&
									std::modf(w, &intpart) == 0.0 &&
									std::modf(h, &intpart) == 0.0)
								{
									wstr = L"x:" + std::to_wstring(int(x)) + L" " +
										L"y:" + std::to_wstring(int(y)) + L" " +
										L"w:" + std::to_wstring(int(w)) + L" " +
										L"h:" + std::to_wstring(int(h));
								}
								else
								{
									wstr = L"x:" + std::to_wstring(x) + L" " +
										L"y:" + std::to_wstring(y) + L" " +
										L"w:" + std::to_wstring(w) + L" " +
										L"h:" + std::to_wstring(h);
								}
								line = L"\t" + wfieldnames[k] + L": " + QStringTowstring(propertyValue.typeName()) + L" " + wstr + L"\n";
							}
							break;
							case QMetaType::QPoint:
							{

							}
							break;
							case QMetaType::QPointF:
							{
								QPointF qpointf = propertyValue.toPointF();
								double x = qpointf.x();
								double y = qpointf.y();
								double intpart;
								std::wstring wstr;
								if (std::modf(x, &intpart) == 0.0 &&
									std::modf(y, &intpart) == 0.0)
								{
									wstr = L"x:" + std::to_wstring(int(x)) + L" " +
										L"y:" + std::to_wstring(int(y));
								}
								else
								{
									wstr = L"x:" + std::to_wstring(x) + L" " +
										L"y:" + std::to_wstring(y);
								}
								line = L"\t" + wfieldnames[k] + L": " + QStringTowstring(propertyValue.typeName()) + L" " + wstr + L"\n";
							}
							break;
							default:
							{
								if (propertyValue.canConvert<QString>())
								{
									line = L"\t" + wfieldnames[k] + L": " + QStringTowstring(propertyValue.typeName()) + L" " + QStringTowstring(propertyValue.toString()) + L"\n";
								}
								else
								{
									line = L"\t" + wfieldnames[k] + L": " + QStringTowstring(propertyValue.typeName()) + L" " + L"handle" + L"\n";
								}
							}
							break;
							}
						}
					}
					msg = msg + line;
				}
				if (!msg.empty()) io->outputMessage(msg);
			}
			io->outputMessage("\n");
		}
	}
	//=============================================================================
	void DispQmlHandleObject(Evaluator *eval, ArrayOf A)
	{
		if (eval != nullptr)
		{
			Interface *io = eval->getInterface();
			if (io)
			{
				if (A.isHandle())
				{
					if (A.isScalar())
					{
						nelson_handle *qp = (nelson_handle*)A.getDataPointer();
						nelson_handle hl = qp[0];
						HandleGenericObject *hlObj = HandleManager::getInstance()->getPointer(hl);
						if (hlObj->getCategory() != L"QML")
						{
							throw Exception(_W("QML handle expected."));
						}
						Dimensions dimsA = A.getDimensions();
						io->outputMessage(L"[QML] - size: ");
						dimsA.printMe(io);
						io->outputMessage("\n");
						io->outputMessage("\n");

						QmlHandleObject *qmlhandleobj = (QmlHandleObject *)hlObj;
						DispQmlHandleObject(io, qmlhandleobj);
					}
					else
					{
						Dimensions dimsA = A.getDimensions();
						io->outputMessage(L"[QML] - size: ");
						dimsA.printMe(io);
						io->outputMessage("\n");
					}
				}
				else
				{
					Error(eval, _W("QML handle expected."));
				}
			}
		}
	}
	//=============================================================================
}
//=============================================================================
