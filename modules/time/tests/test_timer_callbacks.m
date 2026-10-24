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
clear global TIMER_TEST_START_COUNT TIMER_TEST_TICK_COUNT TIMER_TEST_STOP_COUNT
clear global TIMER_TEST_ERROR_COUNT TIMER_TEST_LAST_EVENT_TYPE TIMER_TEST_EXTRA_VALUE
%=============================================================================
global TIMER_TEST_START_COUNT TIMER_TEST_TICK_COUNT TIMER_TEST_STOP_COUNT
global TIMER_TEST_ERROR_COUNT TIMER_TEST_LAST_EVENT_TYPE TIMER_TEST_EXTRA_VALUE
TIMER_TEST_START_COUNT = 0;
TIMER_TEST_TICK_COUNT = 0;
TIMER_TEST_STOP_COUNT = 0;
TIMER_TEST_ERROR_COUNT = 0;
TIMER_TEST_LAST_EVENT_TYPE = '';
TIMER_TEST_EXTRA_VALUE = 0;
%=============================================================================
t = timer('ExecutionMode', 'fixedSpacing', ...
  'Period', 0.02, ...
  'StartDelay', 0.01, ...
  'TasksToExecute', 3, ...
  'StartFcn', 'global TIMER_TEST_START_COUNT; TIMER_TEST_START_COUNT = TIMER_TEST_START_COUNT + 1;', ...
  'TimerFcn', {@timerCallbackRecorder, 42}, ...
  'StopFcn', 'global TIMER_TEST_STOP_COUNT; TIMER_TEST_STOP_COUNT = TIMER_TEST_STOP_COUNT + 1;');
start(t);
wait(t);
%=============================================================================
asserts.isequal(TIMER_TEST_START_COUNT, 1);
asserts.isequal(TIMER_TEST_TICK_COUNT, 3);
asserts.isequal(TIMER_TEST_STOP_COUNT, 1);
asserts.isequal(TIMER_TEST_ERROR_COUNT, 0);
asserts.isequal(TIMER_TEST_LAST_EVENT_TYPE, 'TimerFcn');
asserts.isequal(TIMER_TEST_EXTRA_VALUE, 42);
asserts.isequal(t.Running, 'off');
asserts.isequal(t.TasksExecuted, 3);
asserts.istrue(t.InstantPeriod >= 0);
asserts.istrue(t.AveragePeriod >= 0);
delete(t);
%=============================================================================
TIMER_TEST_STOP_COUNT = 0;
TIMER_TEST_ERROR_COUNT = 0;
errorTimer = timer('TimerFcn', @timerCallbackFailure, ...
  'ErrorFcn', 'global TIMER_TEST_ERROR_COUNT; TIMER_TEST_ERROR_COUNT = TIMER_TEST_ERROR_COUNT + 1;', ...
  'StopFcn', 'global TIMER_TEST_STOP_COUNT; TIMER_TEST_STOP_COUNT = TIMER_TEST_STOP_COUNT + 1;');
start(errorTimer);
wait(errorTimer);
%=============================================================================
asserts.isequal(errorTimer.Running, 'off');
asserts.isequal(TIMER_TEST_ERROR_COUNT, 1);
asserts.isequal(TIMER_TEST_STOP_COUNT, 1);
delete(errorTimer);
clear global TIMER_TEST_START_COUNT TIMER_TEST_TICK_COUNT TIMER_TEST_STOP_COUNT
clear global TIMER_TEST_ERROR_COUNT TIMER_TEST_LAST_EVENT_TYPE TIMER_TEST_EXTRA_VALUE
delete(timerfindall());
%=============================================================================
function timerCallbackRecorder(src, event, extraValue)
  asserts.isequal(class(src), 'timer');
  asserts.istrue(isfield(event, 'Type'));
  asserts.istrue(isfield(event, 'Data'));
  asserts.isequal(event.Type, 'TimerFcn');
  global TIMER_TEST_TICK_COUNT TIMER_TEST_LAST_EVENT_TYPE TIMER_TEST_EXTRA_VALUE
  TIMER_TEST_TICK_COUNT = TIMER_TEST_TICK_COUNT + 1;
  TIMER_TEST_LAST_EVENT_TYPE = event.Type;
  TIMER_TEST_EXTRA_VALUE = extraValue;
end
%=============================================================================
function timerCallbackFailure(src, event)
  asserts.isequal(class(src), 'timer');
  asserts.isequal(event.Type, 'TimerFcn');
  error('timer callback failure');
end
%=============================================================================
