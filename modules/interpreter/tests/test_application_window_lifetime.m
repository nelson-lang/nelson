%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--SEQUENTIAL TEST REQUIRED-->
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Advanced CLI native window lifetime test is not available in the portable runtime.');
original = findall(groot(), 'Type', 'figure');
asserts.istrue(isempty(original));
figures = figure('Visible', 'off');
cleanup = onCleanup(@() delete(findall(groot(), 'Type', 'figure')));
__nelsonc_wait_for_windows__();
asserts.istrue(isgraphics(figures));
set(figures, 'Visible', 'on', 'HandleVisibility', 'off');
clock = timer('StartDelay', 0.05, 'TimerFcn', {@hide_figure, figures});
timerCleanup = onCleanup(@() delete(clock));
start(clock);
__nelsonc_wait_for_windows__();
asserts.istrue(isgraphics(figures));
asserts.isequal(char(get(figures, 'Visible')), 'off');
asserts.isequal(clock.TasksExecuted, 1);
clear timerCleanup;
clear cleanup;

function hide_figure(source, event, f)
  set(f, 'Visible', 'off');
end
