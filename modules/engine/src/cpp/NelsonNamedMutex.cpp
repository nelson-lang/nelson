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
#include <boost/interprocess/sync/named_mutex.hpp>
#include "NelsonNamedMutex.hpp"
#include "Nelson_VERSION.h"
//=============================================================================
namespace Nelson {
	//=============================================================================
	boost::interprocess::named_mutex *nelson_mutex = nullptr;
	//=============================================================================
	bool openNelsonMutex()
	{
		bool res = false;
		if (nelson_mutex == nullptr)
		{
			nelson_mutex = new boost::interprocess::named_mutex(boost::interprocess::create_only, NELSON_VERSION_NMMM_STRING);
			res = true;
		}
		return res;
	}
	//=============================================================================
	bool closeNelsonMutex()
	{
		bool res = false;
		if (nelson_mutex)
		{
			nelson_mutex->remove(NELSON_VERSION_NMMM_STRING);
			delete nelson_mutex;
			nelson_mutex = nullptr;
		}
		return res;
	}
	//=============================================================================
	bool haveNelsonMutex()
	{
		bool res = false;
		try
		{
			boost::interprocess::named_mutex other_nelson_mutex(boost::interprocess::open_only, NELSON_VERSION_NMMM_STRING);
			res = true;
		}
		catch (boost::interprocess::interprocess_exception e)
		{
			res = false;
		}
		return res;
	}
	//=============================================================================
}
//=============================================================================
