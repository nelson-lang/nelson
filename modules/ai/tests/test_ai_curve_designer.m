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
aiCurveFigureVisible = 'off';
aiCurveAnimationEnabled = true;
aiCurveAnimationFrames = 6;
aiCurveFramePause = 0;
aiCurveAnimationContinuous = false;
run([modulepath('ai', 'root'), '/examples/ai_curve_designer.m']);

assert_istrue(isgraphics(aiCurveFigure));
assert_istrue(isgraphics(aiCurveLine));
% The demo picked three frequencies, from the AI or the safe fallback.
assert_isequal(numel(aiCurveParameters), 3);
assert_istrue(all(aiCurveParameters >= 2 & aiCurveParameters <= 7));
% The curve stays inside the unit square of the two sine axes.
ydata = get(aiCurveLine, 'YData');
assert_istrue(max(abs(ydata)) <= 1 + 1e-9);
delete(aiCurveFigure);
drawnow();
%=============================================================================
