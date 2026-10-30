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
matioFigureVisible = 'off';
matioAnimationEnabled = true;
matioFramePause = 0;
matioAnimationContinuous = false;
matioSampleCount = 1200;
run([modulepath('matio', 'root'), '/examples/orbit_replay.m']);

assert_istrue(isgraphics(matioFigure));
% The orbit was saved to and loaded back from a MAT-file.
assert_istrue(isfile(matioFile));
assert_isequal(size(matioPath), [matioSampleCount 3]);
% The round trip preserved the orbit values.
assert_isapprox(matioPath(10, 3), 0.5 * sin(3 * matioTheta(10)), 1e-6);
delete(matioFigure);
drawnow();
%=============================================================================
