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
#include <portaudio.h>
#include "AudioplayerObject.hpp"
#include "ClassName.hpp"
#include "characters_encoding.hpp"
#include "PathFuncManager.hpp"
#include "BuiltInFunctionDefManager.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	AudioplayerObject::AudioplayerObject() : HandleGenericObject(std::wstring(AUDIOPLAYER_CATEGORY_STR), this)
	{
		int defaultOutput = Pa_GetDefaultOutputDevice();
		const PaDeviceInfo *pdi = Pa_GetDeviceInfo(defaultOutput);
		if (pdi)
		{
			_SampleRate = (int)pdi->defaultSampleRate;
			_BitsPerSample = 0;
			_NumberOfChannels = 0;
			_DeviceID = -1;
			_CurrentSample = 0;
			_TotalSamples = 0;
			_Running = false;
			_StartFcn = ArrayOf::emptyConstructor();
			_StopFcn = ArrayOf::emptyConstructor();
			_TimerFcn = ArrayOf::emptyConstructor();
			_TimerPeriod = 1;
			_Tag = L"";
			_UserData = ArrayOf::emptyConstructor();
			_Type = AUDIOPLAYER_CATEGORY_STR;

		}
	}
	//=============================================================================
	AudioplayerObject::~AudioplayerObject()
	{

	}
	//=============================================================================
	bool AudioplayerObject::isWriteableProperty(std::wstring propertyName)
	{
		if (propertyName == L"SampleRate") return true;
		if (propertyName == L"BitsPerSample") return false;
		if (propertyName == L"NumberOfChannels") return false;
		if (propertyName == L"DeviceID") return false;
		if (propertyName == L"CurrentSample") return false;
		if (propertyName == L"TotalSamples") return false;
		if (propertyName == L"Running") return true;
		if (propertyName == L"StartFcn") return true;
		if (propertyName == L"StopFcn") return true;
		if (propertyName == L"TimerFcn") return true;
		if (propertyName == L"TimerPeriod") return true;
		if (propertyName == L"Tag") return true;
		if (propertyName == L"UserData") return true;
		if (propertyName == L"Type") return false;
		return false;
	}
	//=============================================================================
	int AudioplayerObject::getSampleRate()
	{
		return _SampleRate;
	}
	//=============================================================================
	int AudioplayerObject::getBitsPerSample()
	{
		return _BitsPerSample;
	}
	//=============================================================================
	int AudioplayerObject::getNumberOfChannels()
	{
		return _NumberOfChannels;
	}
	//=============================================================================
	int AudioplayerObject::getDeviceID()
	{
		return _DeviceID;
	}
	//=============================================================================
	int AudioplayerObject::getCurrentSample()
	{
		return _CurrentSample;
	}
	//=============================================================================
	int AudioplayerObject::getTotalSamples()
	{
		return _TotalSamples;
	}
	//=============================================================================
	bool AudioplayerObject::getRunning()
	{
		return _Running;
	}
	//=============================================================================
	Nelson::ArrayOf AudioplayerObject::getStartFcn()
	{
		return _StartFcn;
	}
	//=============================================================================
	Nelson::ArrayOf AudioplayerObject::getStopFcn()
	{
		return _StopFcn;
	}
	//=============================================================================
	Nelson::ArrayOf AudioplayerObject::getTimerFcn()
	{
		return _TimerFcn;
	}
	//=============================================================================
	double AudioplayerObject::getTimerPeriod()
	{
		return _TimerPeriod;
	}
	//=============================================================================
	std::wstring AudioplayerObject::getTag()
	{
		return _Tag;
	}
	//=============================================================================
	Nelson::ArrayOf AudioplayerObject::getUserData()
	{
		return _UserData;
	}
	//=============================================================================
	std::wstring AudioplayerObject::getType()
	{
		return _Type;
	}
	//=============================================================================
	bool AudioplayerObject::setSampleRate(int sr)
	{
		_SampleRate = sr;
		return true;
	}
	//=============================================================================
	bool AudioplayerObject::setRunning(bool on)
	{
		_Running = on;
		return true;
	}
	//=============================================================================
	bool AudioplayerObject::setStartFcn(Nelson::ArrayOf funcHandle)
	{
		_StartFcn = funcHandle;
		return true;
	}
	//=============================================================================
	bool AudioplayerObject::setStopFcn(Nelson::ArrayOf funcHandle)
	{
		_StopFcn = funcHandle;
		return true;
	}
	//=============================================================================
	bool AudioplayerObject::setTimerFcn(Nelson::ArrayOf funcHandle)
	{
		_TimerFcn = funcHandle;
		return true;
	}
	//=============================================================================
	bool AudioplayerObject::setTimerPeriod(double period)
	{
		_TimerPeriod = period;
		return true;
	}
	//=============================================================================
	bool AudioplayerObject::setTag(std::wstring tag)
	{
		_Tag = tag;
		return true;
	}
	//=============================================================================
	bool AudioplayerObject::setUserData(Nelson::ArrayOf userData)
	{
		_UserData = userData;
		return true;
	}
	//=============================================================================
	bool AudioplayerObject::disp(Evaluator *eval)
	{
		if (eval != nullptr)
		{
			Interface *io = eval->getInterface();
			if (io)
			{
				std::wstring valueToDisp = L"";
				io->outputMessage(L"\n");

				valueToDisp = std::to_wstring(getSampleRate());
				io->outputMessage(L"\tSampleRate: \t" + valueToDisp + L"\n");

				valueToDisp = std::to_wstring(getBitsPerSample());
				io->outputMessage(L"\tBitsPerSample: \t" + valueToDisp + L"\n");

				valueToDisp = std::to_wstring(getNumberOfChannels());
				io->outputMessage(L"\tNumberOfChannels: \t" + valueToDisp + L"\n");

				valueToDisp = std::to_wstring(getDeviceID());
				io->outputMessage(L"\tDeviceID: \t" + valueToDisp + L"\n");

				valueToDisp = std::to_wstring(getCurrentSample());
				io->outputMessage(L"\tCurrentSample: \t" + valueToDisp + L"\n");

				valueToDisp = std::to_wstring(getTotalSamples());
				io->outputMessage(L"\tTotalSamples: \t" + valueToDisp + L"\n");
				if (getRunning())
				{
					valueToDisp = L"on";
				}
				else
				{
					valueToDisp = L"off";
				}
				io->outputMessage(L"\tRunning: \t" + valueToDisp + L"\n");

				if (_StartFcn.isEmpty())
				{
					valueToDisp = L"[]";
				}
				else
				{
					function_handle fh = _StartFcn.getContentAsFunctionHandle();
					std::wstring functionname;
					bool found = PathFuncManager::getInstance()->find(fh, functionname);
					if (!found)
					{
						found = BuiltInFunctionDefManager::getInstance()->find(fh, functionname);
					}
					if (!functionname.empty())
					{
						valueToDisp = L"@" + functionname;
					}
				}
				io->outputMessage(L"\tStartFcn: \t" + valueToDisp + L"\n");

				if (_StopFcn.isEmpty())
				{
					valueToDisp = L"[]";
				}
				else
				{
					function_handle fh = _StopFcn.getContentAsFunctionHandle();
					std::wstring functionname;
					bool found = PathFuncManager::getInstance()->find(fh, functionname);
					if (!found)
					{
						found = BuiltInFunctionDefManager::getInstance()->find(fh, functionname);
					}
					if (!functionname.empty())
					{
						valueToDisp = L"@" + functionname;
					}
				}
				io->outputMessage(L"\tStopFcn: \t" + valueToDisp + L"\n");

				if (_TimerFcn.isEmpty())
				{
					valueToDisp = L"[]";
				}
				else
				{
					function_handle fh = _TimerFcn.getContentAsFunctionHandle();
					std::wstring functionname;
					bool found = PathFuncManager::getInstance()->find(fh, functionname);
					if (!found)
					{
						found = BuiltInFunctionDefManager::getInstance()->find(fh, functionname);
					}
					if (!functionname.empty())
					{
						valueToDisp = L"@" + functionname;
					}
				}
				io->outputMessage(L"\tTimerFcn: \t" + valueToDisp + L"\n");
				
				valueToDisp = std::to_wstring(getTimerPeriod());
				io->outputMessage(L"\tTimerPeriod: \t" + valueToDisp + L"\n");
				valueToDisp = getTag();
				io->outputMessage(L"\tTag: \t'" + valueToDisp + L"'\n");

				if (_UserData.isEmpty(true))
				{
					valueToDisp = L"[]";
				}
				else
				{
					Dimensions dimsUserData = _UserData.getDimensions();
					std::string userDataClassName;
					ClassName(_UserData, userDataClassName);
					valueToDisp = utf8_to_wstring("[" + userDataClassName + "] - size: " + dimsUserData.toString());
				}
				io->outputMessage(L"\tUserData: \t" + valueToDisp + L"\n");
				
				valueToDisp = getType();
				io->outputMessage(L"\tType: \t'" + valueToDisp + L"'\n");
				
				io->outputMessage(L"\n");
				return true;
			}
		}
		return false;
	}
	//=============================================================================
}
//=============================================================================
