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
%=============================================================================
% The web figure hosts deliver browser input as commands drained between the
% statements of a running script (__web_figure_key_event__,
% __figure_mouse_event__ 'web', __web_uicontrol_action__). Those commands only
% queue the figure callbacks: they run at the next drawnow()/pause(), as native
% window events do, never in the middle of the statement that delivered them.
% So the classic frame loop below (read UserData, work, write it back) cannot
% overwrite a key release delivered in between -- the write-back happens
% before the callbacks run, and the release wins.
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 320 240]);
f.WindowKeyPressFcn = @(source, event) recordTurnKey(source, event, true);
f.WindowKeyReleaseFcn = @(source, event) recordTurnKey(source, event, false);
f.UserData = struct('turnRight', false, 'frame', 0);
%=============================================================================
% frame body: read, input arrives (press then release), write back
state = get(f, 'UserData');
assert_istrue(__web_figure_key_event__(f, true, '', 'ArrowRight', false, false, false, false));
assert_istrue(__web_figure_key_event__(f, false, '', 'ArrowRight', false, false, false, false));
live = get(f, 'UserData');
assert_isfalse(live.turnRight);
state.frame = 1;
set(f, 'UserData', state);
% the frame pacing runs both queued callbacks, in order
pause(0.1);
live = get(f, 'UserData');
assert_isfalse(live.turnRight);
assert_isequal(live.frame, 1);
%=============================================================================
% a press alone survives the same write-back: the callbacks did run
state = get(f, 'UserData');
assert_istrue(__web_figure_key_event__(f, true, '', 'ArrowRight', false, false, false, false));
state.frame = 2;
set(f, 'UserData', state);
drawnow();
live = get(f, 'UserData');
assert_istrue(live.turnRight);
assert_isequal(live.frame, 2);
%=============================================================================
% __web_flush_callbacks__ runs the queued callbacks now and returns how many:
% the web hosts append it to a figure action evaluated while idle (direct RPC)
assert_istrue(__web_figure_key_event__(f, false, '', 'ArrowRight', false, false, false, false));
assert_istrue(__web_figure_key_event__(f, true, '', 'ArrowRight', false, false, false, false));
assert_istrue(__web_figure_key_event__(f, false, '', 'ArrowRight', false, false, false, false));
live = get(f, 'UserData');
assert_istrue(live.turnRight);
assert_isequal(__web_flush_callbacks__(), 3);
live = get(f, 'UserData');
assert_isfalse(live.turnRight);
assert_isequal(__web_flush_callbacks__(), 0);
%=============================================================================
% mouse events through the web entry point queue their callbacks the same way
f.WindowButtonDownFcn = @(source, event) set(source, 'UserData', 'pressed');
f.UserData = 'idle';
assert_istrue(__figure_mouse_event__(f, 'press', [160 120], 'web'));
assert_isequal(f.UserData, 'idle');
drawnow();
assert_isequal(f.UserData, 'pressed');
%=============================================================================
% so does a uicontrol action
button = uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'Apply', ...
  'Callback', @(source, event) set(f, 'UserData', 'applied'));
drawnow();
assert_istrue(__web_uicontrol_action__(button, 0, ''));
assert_isequal(f.UserData, 'pressed');
drawnow();
assert_isequal(f.UserData, 'applied');
close(f);
%=============================================================================
function recordTurnKey(figureHandle, event, pressed)
  state = get(figureHandle, 'UserData');
  if strcmp(event.Key, 'ArrowRight')
    state.turnRight = pressed;
  end
  set(figureHandle, 'UserData', state);
end
%=============================================================================
