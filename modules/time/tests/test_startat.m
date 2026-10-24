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
clear global TIMER_TEST_STARTAT_DIRECT_COUNT
global TIMER_TEST_STARTAT_DIRECT_COUNT
TIMER_TEST_STARTAT_DIRECT_COUNT = 0;
t = timer('TimerFcn', ...
  'global TIMER_TEST_STARTAT_DIRECT_COUNT; TIMER_TEST_STARTAT_DIRECT_COUNT = TIMER_TEST_STARTAT_DIRECT_COUNT + 1;');
startat(t, now() + 0.02 / 86400);
wait(t);
asserts.isequal(TIMER_TEST_STARTAT_DIRECT_COUNT, 1);
delete(t);
clear global TIMER_TEST_STARTAT_DIRECT_COUNT
delete(timerfindall());
%=============================================================================
