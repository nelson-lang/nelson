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
modMultFigureVisible = 'off';
modMultAnimationEnabled = true;
modMultAnimationFrames = 5;
modMultFramePause = 0;
modMultAnimationContinuous = false;
modMultPointCount = 180;
run([nelsonroot(), ...
  '/modules/elementary_mathematics/examples/modular_multiplication_circle.m']);

assert_istrue(isgraphics(modMultFigure));
assert_istrue(isgraphics(modMultLine));
chordX = get(modMultLine, 'XData');
chordY = get(modMultLine, 'YData');
% Each point contributes a source, a target and a NaN separator.
assert_isequal(numel(chordX), 3 * modMultPointCount);
assert_isequal(numel(chordY), 3 * modMultPointCount);
% Chord endpoints stay on the unit circle (ignoring the NaN separators).
radii = hypot(chordX(~isnan(chordX)), chordY(~isnan(chordY)));
assert_istrue(all(abs(radii - 1) < 1e-9));
delete(modMultFigure);
drawnow();
%=============================================================================
