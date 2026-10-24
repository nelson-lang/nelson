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
clear global TIMER_TEST_STARTAT_COUNT
global TIMER_TEST_STARTAT_COUNT
TIMER_TEST_STARTAT_COUNT = 0;
%=============================================================================
t1 = timer('TimerFcn', 'global TIMER_TEST_STARTAT_COUNT; TIMER_TEST_STARTAT_COUNT = TIMER_TEST_STARTAT_COUNT + 1;');
startat(t1, now() + 0.03 / 86400);
wait(t1);
asserts.isequal(TIMER_TEST_STARTAT_COUNT, 1);
delete(t1);
%=============================================================================
v = datevec(now() + 0.03 / 86400);
t2 = timer('TimerFcn', 'global TIMER_TEST_STARTAT_COUNT; TIMER_TEST_STARTAT_COUNT = TIMER_TEST_STARTAT_COUNT + 1;');
startat(t2, v(1), v(2), v(3), v(4), v(5), v(6));
wait(t2);
asserts.isequal(TIMER_TEST_STARTAT_COUNT, 2);
delete(t2);
clear global TIMER_TEST_STARTAT_COUNT
delete(timerfindall());
%=============================================================================
