%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--AUDIO OUTPUT REQUIRED-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
signal = rand(2, 44100) - 0.5;
playObj = audioplayer(signal, 44100, 16);
playObj.StartFcn = @(src, event) disp('Playback started');
playObj.StopFcn = @(src, event) disp('Playback stopped');
playObj.TimerFcn = @(src, event) disp('Playback in progress');
play(playObj)
