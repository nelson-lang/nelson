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
profFigureVisible = 'off';
profAnimationEnabled = true;
profAnimationFrames = 5;
profFramePause = 0;
profAnimationContinuous = false;
run([modulepath('profiler', 'root'), '/examples/profile_flame.m']);

assert_istrue(isgraphics(profFigure));
assert_istrue(isgraphics(profBars(1)));
% profile('info') returned a function table the demo ranked by time.
assert_istrue(numel(profTopNames) >= 1);
assert_istrue(numel(profTopTimes) == numel(profTopNames));
% The bars are sorted from the slowest function downward.
assert_istrue(all(diff(profTopTimes) <= 0));
assert_istrue(profTopTimes(1) > 0);
delete(profFigure);
drawnow();
%=============================================================================
