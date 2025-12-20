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
sleep(5);
stop(r)
%=============================================================================
r = audioplayer(Y', st.fs);
play(r);
sleep(5);
stop(r)
%=============================================================================
r = audioplayer(st.y, st.fs);
f = fieldnames(r);
ref =  {'SampleRate'; ...
    'BitsPerSample'; ...  
    'NumberOfChannels'; ...
    'DeviceID'; ...
    'CurrentSample'; ...   
    'TotalSamples'; ...   
    'Running'; ...        
    'Tag';  ...         
    'UserData';  ...        
    'Type'; ...       
    'StartFcn'; ...        
    'StopFcn'; ...        
    'TimerFcn'; ...       
    'TimerPeriod'};
assert_isequal(f, ref);
assert_isequal(r.SampleRate, 8192);
assert_isequal(r.BitsPerSample, 16);
assert_isequal(r.NumberOfChannels, 1);
assert_isequal(r.DeviceID, -1);
assert_isequal(r.CurrentSample, 0);
assert_isequal(r.TotalSamples, 73113);
assert_isequal(r.Running, 'off');
assert_isequal(r.Tag, '');
assert_isequal(r.UserData, []);
assert_isequal(r.Type, 'audioplayer');
%=============================================================================
if(~isempty(getenv('IN_NIX_SHELL')) && ~ismac())
  return
end
%=============================================================================
play(r)
assert_istrue(isplaying(r))
sleep(5);
play(r)
assert_istrue(isplaying(r))
pause(r)
assert_isfalse(isplaying(r))
play(r)
assert_istrue(isplaying(r))
pause(r)
assert_isfalse(isplaying(r))
resume(r)
assert_istrue(isplaying(r))
stop(r)
assert_isfalse(isplaying(r))
%=============================================================================
