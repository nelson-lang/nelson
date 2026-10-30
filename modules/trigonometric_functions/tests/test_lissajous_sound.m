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
lissajousFigureVisible = 'off';
lissajousAnimationEnabled = true;
lissajousAnimationFrames = 8;
lissajousFramePause = 0;
lissajousAnimationContinuous = false;
lissajousSoundEnabled = false;
lissajousFreqA = 3;
lissajousFreqB = 2;
run([modulepath('trigonometric_functions', 'root'), ...
  '/examples/lissajous_sound.m']);

assert_istrue(isgraphics(lissajousFigure));
assert_istrue(isgraphics(lissajousCurve));
xdata = get(lissajousCurve, 'XData');
ydata = get(lissajousCurve, 'YData');
assert_isequal(numel(xdata), numel(lissajousT));
assert_isequal(numel(ydata), numel(lissajousT));
% The curve stays inside the unit square of the two sine axes.
assert_istrue(max(abs(xdata)) <= 1 + 1e-9);
assert_istrue(max(abs(ydata)) <= 1 + 1e-9);
delete(lissajousFigure);
drawnow();
%=============================================================================
