%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
% StopFcn runs exactly once per play, on natural end as on stop.
%=============================================================================
global AUDIOPLAYER_STOPFCN_COUNT
fs = 44100;
short = 0.02 * sin(2 * pi * 440 * (0:round(0.05 * fs) - 1)' / fs);
long = 0.02 * sin(2 * pi * 440 * (0:fs - 1)' / fs);
%=============================================================================
AUDIOPLAYER_STOPFCN_COUNT = 0;
p = audioplayer(short, fs);
p.StopFcn = 'global AUDIOPLAYER_STOPFCN_COUNT; AUDIOPLAYER_STOPFCN_COUNT = AUDIOPLAYER_STOPFCN_COUNT + 1;';
play(p);
pause(0.5);
stop(p);
pause(0.1);
asserts.isequal(AUDIOPLAYER_STOPFCN_COUNT, 1);
delete(p);
%=============================================================================
AUDIOPLAYER_STOPFCN_COUNT = 0;
p = audioplayer(long, fs);
p.StopFcn = 'global AUDIOPLAYER_STOPFCN_COUNT; AUDIOPLAYER_STOPFCN_COUNT = AUDIOPLAYER_STOPFCN_COUNT + 1;';
play(p);
pause(0.2);
stop(p);
stop(p);
pause(0.2);
asserts.isequal(AUDIOPLAYER_STOPFCN_COUNT, 1);
delete(p);
%=============================================================================
AUDIOPLAYER_STOPFCN_COUNT = 0;
p = audioplayer(short, fs);
p.StopFcn = 'global AUDIOPLAYER_STOPFCN_COUNT; AUDIOPLAYER_STOPFCN_COUNT = AUDIOPLAYER_STOPFCN_COUNT + 1;';
playblocking(p);
pause(0.1);
asserts.isequal(AUDIOPLAYER_STOPFCN_COUNT, 1);
delete(p);
%=============================================================================
clear global AUDIOPLAYER_STOPFCN_COUNT
%=============================================================================
