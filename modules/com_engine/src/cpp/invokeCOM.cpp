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
#include <atlconv.h>
#include "invokeCOM.hpp"
#include "Exception.hpp"
//=============================================================================
bool invokeCom(int autoType, VARIANT *pvResult, std::wstring &errorMessage, IDispatch *pDisp, std::wstring propertyName, int cArgs ...)
{
	VARIANT *pArgs = NULL;
	va_list marker;
	va_start(marker, cArgs);
	EXCEPINFO excepinfo;

	if (!pDisp)
	{
		errorMessage = L"IDispatch not defined.";
		return false;
	}

	DISPPARAMS dp = { NULL, NULL, 0, 0 };
	DISPID dispidNamed = DISPID_PROPERTYPUT;
	DISPID dispID;
	HRESULT hr;

	LPOLESTR name = W2OLE((wchar_t*)propertyName.c_str());

	hr = pDisp->GetIDsOfNames(IID_NULL, &name, 1, LOCALE_USER_DEFAULT, &dispID);
	if (FAILED(hr))
	{
		errorMessage = std::wstring(L"Error detected (code):") + L"method not found." + L"\n";
		return false;
	}

	try
	{
		pArgs = new VARIANT[cArgs + 1];
	}
	catch (std::bad_alloc &e)
	{
		errorMessage = std::wstring(ERROR_MEMORY_ALLOCATION);
		return false;
	}

	if (pArgs == nullptr)
	{
		errorMessage = std::wstring(ERROR_MEMORY_ALLOCATION);
		return false;
	}

	for (int i = 0; i < cArgs; i++)
	{
		pArgs[i] = va_arg(marker, VARIANT);
	}
	va_end(marker);

	dp.cArgs = cArgs;
	dp.rgvarg = pArgs;

	if (autoType & DISPATCH_PROPERTYPUT)
	{
		dp.cNamedArgs = 1;
		dp.rgdispidNamedArgs = &dispidNamed;
	}

	bool invokeFails = false;
	try
	{
		hr = pDisp->Invoke(dispID, IID_NULL, LOCALE_SYSTEM_DEFAULT, (WORD)autoType, &dp, pvResult, &excepinfo, NULL);
	}
	catch (std::runtime_error &e)
	{
		invokeFails = true;
	}

	for (int i = 0; i < cArgs; i++)
	{
		VariantClear(&pArgs[i]);
	}
	delete[] pArgs;
	pArgs = NULL;

	if (FAILED(hr) || invokeFails)
	{
		BSTR bs = excepinfo.bstrSource;
		std::wstring ws1(bs, SysStringLen(bs));
		errorMessage = std::wstring(L"Error detected (Source):") + ws1 + L"\n";

		bs = excepinfo.bstrDescription;
		std::wstring ws2(bs, SysStringLen(bs));
		errorMessage = errorMessage + std::wstring(L"Error detected (Description):") + ws2 + L"\n";

		bs = excepinfo.bstrHelpFile;
		std::wstring ws3(bs, SysStringLen(bs));
		errorMessage = errorMessage + std::wstring(L"Error detected (Help File):") + ws3 + L"\n";

		switch (hr)
		{
		case DISP_E_BADPARAMCOUNT:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_BADPARAMCOUNT" + L"\n";
		}
		break;
		case DISP_E_BADVARTYPE:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_BADVARTYPE" + L"\n";
		}
		break;
		case DISP_E_EXCEPTION:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_EXCEPTION" + L"\n";
		}
		break;
		case DISP_E_MEMBERNOTFOUND:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_MEMBERNOTFOUND" + L"\n";
		}
		break;
		case DISP_E_NONAMEDARGS:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_NONAMEDARGS" + L"\n";
		}
		break;
		case DISP_E_OVERFLOW:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_OVERFLOW" + L"\n";
		}
		break;
		case DISP_E_PARAMNOTFOUND:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_PARAMNOTFOUND" + L"\n";
		}
		break;
		case DISP_E_TYPEMISMATCH:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_TYPEMISMATCH" + L"\n";
		}
		break;
		case DISP_E_UNKNOWNINTERFACE:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_UNKNOWNINTERFACE" + L"\n";
		}
		break;
		case DISP_E_UNKNOWNLCID:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_UNKNOWNLCID" + L"\n";
		}
		break;
		case DISP_E_PARAMNOTOPTIONAL:
		{
			errorMessage = errorMessage + std::wstring(L"Error detected (code):") + L"DISP_E_PARAMNOTOPTIONAL" + L"\n";
		}
		break;
		}
		return false;
	}
	return true;
}
//=============================================================================
