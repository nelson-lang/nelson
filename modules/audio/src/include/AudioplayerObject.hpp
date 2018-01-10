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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "HandleGenericObject.hpp"
#include "nlsAudio_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
	//=============================================================================
	#define AUDIOPLAYER_CATEGORY_STR L"audioplayer"
	//=============================================================================
	class NLSAUDIO_IMPEXP AudioplayerObject : public HandleGenericObject {
	public:
		AudioplayerObject();
		~AudioplayerObject();
		bool isWriteableProperty(std::wstring propertyName);
		// getter
		int getSampleRate();
		int getBitsPerSample();
		int getNumberOfChannels();
		int getDeviceID();
		int getCurrentSample();
		int getTotalSamples();
		bool getRunning();
		Nelson::ArrayOf getStartFcn();
		Nelson::ArrayOf getStopFcn();
		Nelson::ArrayOf getTimerFcn();
		double getTimerPeriod();
		std::wstring getTag();
		Nelson::ArrayOf getUserData();
		std::wstring getType();

		// setter
		bool setSampleRate(int sr);
		bool setRunning(bool on);
		bool setStartFcn(Nelson::ArrayOf funcHandle);
		bool setStopFcn(Nelson::ArrayOf funcHandle);
		bool setTimerFcn(Nelson::ArrayOf funcHandle);
		bool setTimerPeriod(double period);
		bool setTag(std::wstring tag);
		bool setUserData(Nelson::ArrayOf userData);

		// disp
		bool disp(Evaluator *eval);

	private:
		int _SampleRate;
		int _BitsPerSample;
		int _NumberOfChannels;
		int _DeviceID;
		int _CurrentSample;
		int _TotalSamples;
		bool _Running;
		ArrayOf _StartFcn;
		ArrayOf _StopFcn;
		ArrayOf _TimerFcn;
		double _TimerPeriod;
		std::wstring _Tag;
		ArrayOf _UserData;
		std::wstring _Type;

	};
	//=============================================================================
}
//=============================================================================
