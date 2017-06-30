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
#include <Windows.h>
#include <Ole2.h>
#include <atlconv.h>
#include <boost/algorithm/string.hpp>
#include "ActiveXServer.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	ComHandleObject *ActiveXServer(std::wstring progId, std::wstring machine)
	{
		IDispatch * pdispApplication = nullptr;
		CLSID clsApplication;
		ComHandleObject *res = nullptr;

		if (boost::algorithm::starts_with(progId, L"{"))
		{
			if (FAILED(CLSIDFromString(progId.c_str(), &clsApplication)))
			{
				throw Exception(_W("Error CLSIDFromString."));
			}
		}
		else
		{
			if (FAILED(CLSIDFromProgID(progId.c_str(), &clsApplication)))
			{
				throw Exception(_W("Error CLSIDFromProgID."));
			}
		}

		if (machine != L"")
		{
			COSERVERINFO ServerInfo;
			ZeroMemory(&ServerInfo, sizeof(COSERVERINFO));

			COAUTHINFO athn;
			ZeroMemory(&athn, sizeof(COAUTHINFO));

			athn.dwAuthnLevel = RPC_C_AUTHN_LEVEL_NONE;
			athn.dwAuthnSvc = RPC_C_AUTHN_WINNT;
			athn.dwAuthzSvc = RPC_C_AUTHZ_NONE;
			athn.dwCapabilities = EOAC_NONE;
			athn.dwImpersonationLevel = RPC_C_IMP_LEVEL_IMPERSONATE;
			athn.pAuthIdentityData = nullptr;
			athn.pwszServerPrincName = nullptr;

			ServerInfo.pwszName = &machine[0];
			ServerInfo.pAuthInfo = &athn;
			ServerInfo.dwReserved1 = 0;
			ServerInfo.dwReserved2 = 0;

			MULTI_QI qi = { 0,0,0 };
			ZeroMemory(&qi, sizeof(MULTI_QI));
			qi.pIID = &IID_IDispatch;

			if (FAILED(CoCreateInstanceEx(clsApplication, NULL, CLSCTX_REMOTE_SERVER, &ServerInfo, 1, &qi)))
			{
				throw Exception(_W("Error CoCreateInstanceEx."));
			}
			pdispApplication = (IDispatch*)qi.pItf;
		}
		else
		{
			if (FAILED(CoCreateInstance(clsApplication, NULL, CLSCTX_SERVER, IID_IDispatch, (void **)&pdispApplication)))
			{
				throw Exception(_W("Error CoCreateInstanceEx."));
			}
		}

		VARIANT *pVariantApplication;
		try
		{
			pVariantApplication = new VARIANT;
		}
		catch (std::bad_alloc)
		{
			throw Exception(ERROR_MEMORY_ALLOCATION);
		}

		VariantInit(pVariantApplication);
		pVariantApplication->vt = VT_DISPATCH;
		pVariantApplication->pdispVal = pdispApplication;

		res = new ComHandleObject(pVariantApplication);
		return res;
	}
	//=============================================================================
	ComHandleObject *GetRunningActiveXServer(std::wstring progId)
	{
		IUnknown *pUnknown;
		CLSID clsApplication;
		IDispatch * pdispApplication = nullptr;
		VARIANT *pVariantApplication = nullptr;
		HRESULT hRes = S_FALSE;

		LPOLESTR idName = W2OLE((wchar_t*)progId.c_str());

		if (progId[0] == L'{')
		{
			if (FAILED(CLSIDFromString(idName, &clsApplication)))
			{
				throw Exception(_W("Invalid PROGID."));
			}
		}
		else
		{
			if (FAILED(CLSIDFromProgID(idName, &clsApplication)))
			{
				throw Exception(_W("Invalid PROGID."));
			}
		}


		hRes = GetActiveObject(clsApplication, NULL, &pUnknown);
		if (FAILED(hRes))
		{
			throw Exception(_W("Server is not running on this system."));
		}

		hRes = pUnknown->QueryInterface(IID_IDispatch, (void **)&pdispApplication);
		pUnknown->Release();

		if (FAILED(hRes))
		{
			throw Exception(_W("Fails to connect to server."));
		}

		try
		{
			pVariantApplication = new VARIANT;
		}
		catch (std::bad_alloc)
		{
			throw Exception(ERROR_MEMORY_ALLOCATION);
		}
		VariantInit(pVariantApplication);
		pVariantApplication->vt = VT_DISPATCH;
		pVariantApplication->pdispVal = pdispApplication;
		ComHandleObject* res = new ComHandleObject(pVariantApplication);
		return res;
	}
	//=============================================================================
}
//=============================================================================
