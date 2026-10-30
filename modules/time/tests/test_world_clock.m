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
worldClockFigureVisible = 'off';
worldClockAnimationEnabled = true;
worldClockAnimationFrames = 4;
worldClockFramePause = 0;
worldClockAnimationContinuous = false;
run([modulepath('time', 'root'), '/examples/world_clock.m']);

assert_istrue(isgraphics(worldClockFigure));
assert_isequal(numel(worldClockHands), worldClockCount);
% Each clock carries an hour, minute and second hand.
firstHands = worldClockHands{1};
assert_istrue(isgraphics(firstHands.hour));
assert_istrue(isgraphics(firstHands.minute));
assert_istrue(isgraphics(firstHands.second));
% The second hand is anchored at the centre and has unit-scale length.
secondX = get(firstHands.second, 'XData');
secondY = get(firstHands.second, 'YData');
assert_isequal(secondX(1), 0);
assert_istrue(hypot(secondX(2), secondY(2)) <= 0.86 + 1e-9);
delete(worldClockFigure);
drawnow();
%=============================================================================
