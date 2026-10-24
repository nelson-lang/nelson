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
clear global TIMER_TEST_WAIT_COUNT
global TIMER_TEST_WAIT_COUNT
TIMER_TEST_WAIT_COUNT = 0;
t = timer('StartDelay', 0.01, ...
  'TimerFcn', 'global TIMER_TEST_WAIT_COUNT; TIMER_TEST_WAIT_COUNT = TIMER_TEST_WAIT_COUNT + 1;');
start(t);
wait(t);
asserts.isequal(TIMER_TEST_WAIT_COUNT, 1);
delete(t);
clear global TIMER_TEST_WAIT_COUNT
delete(timerfindall());
%=============================================================================
