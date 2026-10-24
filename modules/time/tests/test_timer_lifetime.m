%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
delete(timerfindall());
clear global TIMER_LIFETIME_COUNT
global TIMER_LIFETIME_COUNT
TIMER_LIFETIME_COUNT = 0;
%=============================================================================
spawnUnstartedTimer();
unstarted = timerfindall('Tag', 'scope_unstarted_timer');
asserts.isequal(numel(unstarted), 1);
asserts.isequal(unstarted.Running, 'off');
delete(unstarted);
%=============================================================================
spawnStoppedTimer();
stopped = timerfindall('Tag', 'scope_stopped_timer');
asserts.isequal(numel(stopped), 1);
asserts.isequal(stopped.Running, 'off');
delete(stopped);
%=============================================================================
spawnStartedTimer();
started = timerfindall('Tag', 'scope_started_timer');
asserts.isequal(numel(started), 1);
asserts.isequal(started.Running, 'on');
wait(started);
started = timerfindall('Tag', 'scope_started_timer');
asserts.isequal(numel(started), 1);
asserts.isequal(started.Running, 'off');
asserts.isequal(started.TasksExecuted, 1);
asserts.isequal(TIMER_LIFETIME_COUNT, 1);
delete(started);
%=============================================================================
asserts.isequal(numel(timerfindall()), 0);
clear global TIMER_LIFETIME_COUNT
delete(timerfindall());
%=============================================================================
function spawnUnstartedTimer()
  clock = timer('Tag', 'scope_unstarted_timer', 'TimerFcn', @timerLifetimeCallback); %#ok<NASGU>
end
%=============================================================================
function spawnStoppedTimer()
  clock = timer('Tag', 'scope_stopped_timer', 'StartDelay', 0.2, ...
    'TimerFcn', @timerLifetimeCallback);
  start(clock);
  stop(clock);
end
%=============================================================================
function spawnStartedTimer()
  clock = timer('Tag', 'scope_started_timer', 'StartDelay', 0.05, ...
    'TimerFcn', @timerLifetimeCallback);
  start(clock);
end
%=============================================================================
function timerLifetimeCallback(source, event)
  asserts.isequal(class(source), 'timer');
  asserts.isequal(event.Type, 'TimerFcn');
  global TIMER_LIFETIME_COUNT
  TIMER_LIFETIME_COUNT = TIMER_LIFETIME_COUNT + 1;
end
%=============================================================================
