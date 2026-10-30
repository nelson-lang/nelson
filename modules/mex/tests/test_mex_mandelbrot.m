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
skip_testsuite(~havecompiler(), 'MEX Mandelbrot demo requires a C compiler.');
%=============================================================================
mexMandelFigureVisible = 'off';
mexMandelAnimationEnabled = true;
mexMandelAnimationFrames = 3;
mexMandelFramePause = 0;
mexMandelAnimationContinuous = false;
mexMandelResolution = 120;
run([modulepath('mex', 'root'), '/examples/mex_mandelbrot.m']);

assert_istrue(isgraphics(mexMandelFigure));
assert_istrue(isgraphics(mexMandelImage));
% The compiled kernel returned the escape-count grid.
assert_isequal(size(mexMandelCounts), [mexMandelResolution mexMandelResolution]);
% Escape counts stay within the iteration budget and include interior points.
assert_istrue(max(mexMandelCounts(:)) <= mexMandelMaxIterations);
assert_istrue(max(mexMandelCounts(:)) >= mexMandelMaxIterations - 1);
delete(mexMandelFigure);
drawnow();
%=============================================================================
