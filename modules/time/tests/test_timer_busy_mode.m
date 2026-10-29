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
skip_testsuite(iswasm(), ...
  'WebAssembly timer callbacks are cooperatively pumped and do not implement native BusyMode overrun timing.');
%=============================================================================
delete(timerfindall());
clear global TIMER_BUSY_TICKS TIMER_BUSY_ERRORS TIMER_BUSY_STOPS
global TIMER_BUSY_TICKS TIMER_BUSY_ERRORS TIMER_BUSY_STOPS
%=============================================================================
TIMER_BUSY_TICKS = 0;
TIMER_BUSY_ERRORS = 0;
TIMER_BUSY_STOPS = 0;
tError = timer('ExecutionMode', 'fixedRate', ...
  'Period', 0.01, ...
  'TasksToExecute', 5, ...
  'BusyMode', 'error', ...
  'TimerFcn', 'global TIMER_BUSY_TICKS; TIMER_BUSY_TICKS = TIMER_BUSY_TICKS + 1; sleep(0.05);', ...
  'ErrorFcn', 'global TIMER_BUSY_ERRORS; TIMER_BUSY_ERRORS = TIMER_BUSY_ERRORS + 1;', ...
  'StopFcn', 'global TIMER_BUSY_STOPS; TIMER_BUSY_STOPS = TIMER_BUSY_STOPS + 1;');
start(tError);
wait(tError);
% overrun stops the timer early; the exact tick count before the error is timing sensitive
asserts.istrue(TIMER_BUSY_TICKS >= 1 && TIMER_BUSY_TICKS < 5);
asserts.isequal(TIMER_BUSY_ERRORS, 1);
asserts.isequal(TIMER_BUSY_STOPS, 1);
delete(tError);
%=============================================================================
TIMER_BUSY_TICKS = 0;
TIMER_BUSY_ERRORS = 0;
tQueue = timer('ExecutionMode', 'fixedRate', ...
  'Period', 0.01, ...
  'TasksToExecute', 5, ...
  'BusyMode', 'queue', ...
  'TimerFcn', 'global TIMER_BUSY_TICKS; TIMER_BUSY_TICKS = TIMER_BUSY_TICKS + 1; sleep(0.03);', ...
  'ErrorFcn', 'global TIMER_BUSY_ERRORS; TIMER_BUSY_ERRORS = TIMER_BUSY_ERRORS + 1;');
start(tQueue);
wait(tQueue);
asserts.isequal(TIMER_BUSY_TICKS, 5);
asserts.isequal(TIMER_BUSY_ERRORS, 0);
queueInstantPeriod = tQueue.InstantPeriod;
asserts.istrue(isfinite(queueInstantPeriod));
asserts.istrue(queueInstantPeriod >= 0);
delete(tQueue);
%=============================================================================
TIMER_BUSY_TICKS = 0;
tDrop = timer('ExecutionMode', 'fixedRate', ...
  'Period', 0.01, ...
  'TasksToExecute', 4, ...
  'BusyMode', 'drop', ...
  'TimerFcn', 'global TIMER_BUSY_TICKS; TIMER_BUSY_TICKS = TIMER_BUSY_TICKS + 1; sleep(0.03);');
start(tDrop);
wait(tDrop);
asserts.isequal(TIMER_BUSY_TICKS, 4);
asserts.istrue(isfinite(tDrop.InstantPeriod));
asserts.istrue(tDrop.InstantPeriod >= tDrop.Period);
delete(tDrop);
%=============================================================================
clear global TIMER_BUSY_TICKS TIMER_BUSY_ERRORS TIMER_BUSY_STOPS
delete(timerfindall());
%=============================================================================
