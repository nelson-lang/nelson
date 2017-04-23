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
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	static nelson_handle hash_gen = 1;
	//=============================================================================
	HandleManager* HandleManager::m_pInstance = nullptr;
	//=============================================================================
	HandleManager *HandleManager::getInstance()
	{
		if (m_pInstance == nullptr)
		{
			m_pInstance = new HandleManager();
		}
		return m_pInstance;
	}
	//=============================================================================
	HandleManager::HandleManager()
	{
		hash_gen = 1;
		handleMap.clear();
	}
	//=============================================================================
	void HandleManager::destroy()
	{
		handleMap.clear();
	}
	//=============================================================================
	nelson_handle HandleManager::addHandle(HandleGenericObject *ptr)
	{
		if (ptr == nullptr)
		{
			return (nelson_handle)0;
		}
		boost::unordered_map<nelson_handle, HandleGenericObject *>::iterator it = handleMap.begin();
		while (it != handleMap.end())
		{
			if (it->second == ptr)
			{
				return it->first;
			}
			++it;
		}
		nelson_handle id = hash_gen + 1;
		handleMap.emplace(id, ptr);
		hash_gen = hash_gen + 1;
		return id;
	}
	//=============================================================================
	bool HandleManager::removeHandle(nelson_handle hl)
	{
		boost::unordered_map<nelson_handle, HandleGenericObject *>::iterator it = handleMap.find(hl);
		if (it != handleMap.end())
		{
			if (it->second != nullptr)
			{
				delete it->second;
				it->second = nullptr;
			}
			handleMap.erase(it);
			return true;
		}
		return false;
	}
	//=============================================================================
	HandleGenericObject *HandleManager::getPointer(nelson_handle hl)
	{
		boost::unordered_map<nelson_handle, HandleGenericObject *>::iterator it = handleMap.find(hl);
		if (it != handleMap.end())
		{
			return it->second;
		}
		return nullptr;
	}
	//=============================================================================
	bool HandleManager::isValid(nelson_handle hl)
	{
		boost::unordered_map<nelson_handle, HandleGenericObject *>::iterator it = handleMap.find(hl);
		if (it != handleMap.end())
		{
			return true;
		}
		return false;
	}
	//=============================================================================
}
//=============================================================================
