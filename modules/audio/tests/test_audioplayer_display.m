%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
signal = rand(2, 44100) - 0.5;
playObj = audioplayer(signal, 44100, 16);
%=============================================================================
R = evalc('disp(playObj)');
REF = '  1×1 handle [audioplayer] 

	SampleRate: 	44100
	BitsPerSample: 	16
	NumberOfChannels: 	2
	DeviceID: 	-1
	CurrentSample: 	0
	TotalSamples: 	44100
	Running: 	off
	StartFcn: 	''''
	StopFcn: 	''''
	TimerFcn: 	''''
	TimerPeriod: 	0.050000
	Tag: 	''''
	UserData: 	[]
	Type: 	''audioplayer''
';
assert_isequal(R, REF)
%=============================================================================
R = evalc('display(playObj)');
REF =  '
playObj =

  1×1 handle [audioplayer] 

	SampleRate: 	44100
	BitsPerSample: 	16
	NumberOfChannels: 	2
	DeviceID: 	-1
	CurrentSample: 	0
	TotalSamples: 	44100
	Running: 	off
	StartFcn: 	''''
	StopFcn: 	''''
	TimerFcn: 	''''
	TimerPeriod: 	0.050000
	Tag: 	''''
	UserData: 	[]
	Type: 	''audioplayer''

';
assert_isequal(R, REF)
%=============================================================================
