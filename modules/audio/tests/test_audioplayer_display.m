%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
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
	Tag: 	''''
	UserData: 	[]
	Type: 	''audioplayer''
';
assert_isequal(R, REF)
%=============================================================================
R = evalc('display(playObj)');
REF = '
playObj =

  1×1 handle [audioplayer] 

	SampleRate: 	44100
	BitsPerSample: 	16
	NumberOfChannels: 	2
	DeviceID: 	-1
	CurrentSample: 	0
	TotalSamples: 	44100
	Running: 	off
	Tag: 	''''
	UserData: 	[]
	Type: 	''audioplayer''

';
assert_isequal(R, REF)
%=============================================================================
