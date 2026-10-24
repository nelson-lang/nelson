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
json_audio = [modulepath('audio', 'tests'), '/test_audioplayer.json'];
st = jsondecode(fileread(json_audio));
%=============================================================================
Y = [st.y, st.y];
try
  r = audioplayer(Y, st.fs);
catch ex
  skip_testsuite(strcmp(ex.message, 'Device unavailable'), 'Device unavailable');
end
%=============================================================================
play(r);
sleep(0.2);
stop(r)
%=============================================================================
r = audioplayer(Y', st.fs);
play(r);
sleep(0.2);
stop(r)
%=============================================================================
r = audioplayer(st.y, st.fs);
f = fieldnames(r);
ref = {'SampleRate'; ...
    'BitsPerSample'; ...
    'NumChannels'; ...
    'DeviceID'; ...
    'CurrentSample'; ...
    'TotalSamples'; ...
    'Running'; ...
    'StartFcn'; ...
    'StopFcn'; ...
    'TimerFcn'; ...
    'TimerPeriod'; ...
    'Tag'; ...
    'UserData'; ...
    'Type'};
asserts.isequal(f, ref);
asserts.isequal(r.SampleRate, 8192);
asserts.isequal(r.BitsPerSample, 16);
asserts.isequal(r.NumChannels, 1);
asserts.isequal(r.DeviceID, -1);
asserts.isequal(r.CurrentSample, 0);
asserts.isequal(r.TotalSamples, 73113);
asserts.isequal(r.Running, 'off');
asserts.isequal(r.Tag, '');
asserts.isequal(r.UserData, []);
asserts.isequal(r.Type, 'audioplayer');
%=============================================================================
if (~isempty(getenv('IN_NIX_SHELL')) && ~ismac())
  return
end
%=============================================================================
play(r)
skip_testsuite(~isplaying(r), 'Audio output unavailable');
asserts.istrue(isplaying(r))
sleep(0.2);
play(r)
skip_testsuite(~isplaying(r), 'Audio output unavailable');
asserts.istrue(isplaying(r))
pause(r)
asserts.isfalse(isplaying(r))
play(r)
skip_testsuite(~isplaying(r), 'Audio output unavailable');
asserts.istrue(isplaying(r))
pause(r)
asserts.isfalse(isplaying(r))
resume(r)
skip_testsuite(~isplaying(r), 'Audio output unavailable');
asserts.istrue(isplaying(r))
stop(r)
asserts.isfalse(isplaying(r))
%=============================================================================
