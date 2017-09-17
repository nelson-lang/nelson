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
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#include "Exist.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	static bool isFile(std::wstring filename)
	{
		boost::filesystem::path data_dir(filename);
		bool bRes = false;
		try
		{
			bRes = boost::filesystem::exists(data_dir) && !boost::filesystem::is_directory(data_dir);
		}
		catch (const boost::filesystem::filesystem_error& e)
		{
			if (e.code() == boost::system::errc::permission_denied)
			{
			}
			bRes = false;
		}
		return bRes;
	}
	//=============================================================================
	static bool isDir(std::wstring path)
	{
		boost::filesystem::path data_dir(path);
		bool bRes = false;
		try
		{
			bRes = boost::filesystem::is_directory(data_dir);
		}
		catch (const boost::filesystem::filesystem_error& e)
		{
			if (e.code() == boost::system::errc::permission_denied)
			{
			}
			bRes = false;
		}
		return bRes;
	}
	//=============================================================================
	int Exist(Evaluator* eval, std::wstring name, std::wstring category)
	{
		int res = 0;
		std::string uname = wstring_to_utf8(name);
		if (category == L"var")
		{
			if (eval->getContext()->isVariable(uname) == true)
			{
				res = NELSON_EXIST_TYPE::var;
			}
		}
		else if (category == L"builtin")
		{
			FuncPtr pfun;
			if (eval->lookupFunction(uname, pfun) == true)
			{
				res = NELSON_EXIST_TYPE::builtin;
			}
		}
		else if (category == L"dir")
		{
			if (isDir(name))
			{
				res = NELSON_EXIST_TYPE::dir;
			}
		}
		else if (category == L"file")
		{
			if (isFile(name))
			{
				res = NELSON_EXIST_TYPE::file;
			}
		}
		return res;
	}
	//=============================================================================
	NLSCORE_IMPEXP int Exist(Evaluator* eval, std::wstring name)
	{
		int res = NELSON_EXIST_TYPE::unknown;
		std::string uname = wstring_to_utf8(name);
		if (Exist(eval, name, L"file") != NELSON_EXIST_TYPE::unknown)
		{
			return NELSON_EXIST_TYPE::file;
		}
		if (Exist(eval, name, L"dir") != NELSON_EXIST_TYPE::unknown)
		{
			return NELSON_EXIST_TYPE::dir;
		}
		if (Exist(eval, name, L"var") != NELSON_EXIST_TYPE::unknown)
		{
			return NELSON_EXIST_TYPE::var;
		}
		if (Exist(eval, name, L"builtin") != NELSON_EXIST_TYPE::unknown)
		{
			return NELSON_EXIST_TYPE::builtin;
		}
		return res;
	}
	//=============================================================================
}
//=============================================================================
