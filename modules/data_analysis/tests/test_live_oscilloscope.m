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
scopeFigureVisible = 'off';
scopeAnimationEnabled = true;
scopeAnimationFrames = 120;
scopeFramePause = 0;
scopeAnimationContinuous = false;
scopeSampleRate = 250;
scopeBpm = 72;
run([modulepath('data_analysis', 'root'), ...
  '/examples/live_oscilloscope.m']);

assert_istrue(isgraphics(scopeFigure));
assert_istrue(isgraphics(scopeSmoothLine));
assert_istrue(isgraphics(scopePeakMarks));
% The smoothed trace covers the whole displayed window.
assert_isequal(numel(get(scopeSmoothLine, 'YData')), scopeWindowLength);
% A five second window at 72 bpm holds about six beats; detection finds them.
peakCount = numel(get(scopePeakMarks, 'XData'));
assert_istrue(peakCount >= 4 && peakCount <= 8);
delete(scopeFigure);
drawnow();
%=============================================================================
