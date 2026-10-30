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
scrollerFigureVisible = 'off';
scrollerAnimationEnabled = true;
scrollerAnimationFrames = 10;
scrollerFramePause = 0;
scrollerAnimationContinuous = false;
scrollerWindow = 24;
run([modulepath('string', 'root'), '/examples/sine_scroller.m']);

assert_istrue(isgraphics(scrollerFigure));
assert_isequal(numel(scrollerChars), scrollerWindow);
assert_istrue(all(isgraphics(scrollerChars)));
% The marquee was padded so it is at least one window longer than its text.
assert_istrue(scrollerLength >= scrollerWindow);
% Each visible slot holds a single character.
assert_isequal(numel(get(scrollerChars(1), 'String')), 1);
delete(scrollerFigure);
drawnow();
%=============================================================================
