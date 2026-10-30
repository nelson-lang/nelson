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
juliaAvailable = false;
try
  jlrun('1 + 1');
  juliaAvailable = true;
catch
end
skip_testsuite(~juliaAvailable, 'Julia-set demo requires a Julia engine.');
%=============================================================================
juliaFigureVisible = 'off';
juliaAnimationEnabled = true;
juliaAnimationFrames = 3;
juliaFramePause = 0;
juliaAnimationContinuous = false;
juliaResolution = 120;
run([modulepath('julia_engine', 'root'), '/examples/julia_set_morph.m']);

assert_istrue(isgraphics(juliaFigure));
assert_istrue(isgraphics(juliaImage));
% Julia returned the escape-count grid that Nelson converted to a double.
assert_isequal(size(juliaGrid), [juliaResolution juliaResolution]);
assert_isequal(class(juliaGrid), 'double');
% Escape counts stay within the iteration budget.
assert_istrue(max(juliaGrid(:)) <= juliaMaxIterations);
assert_istrue(max(juliaGrid(:)) > 0);
delete(juliaFigure);
drawnow();
%=============================================================================
