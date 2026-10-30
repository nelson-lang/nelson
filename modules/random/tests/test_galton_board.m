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
galtonFigureVisible = 'off';
galtonAnimationEnabled = true;
galtonAnimationFrames = 120;
galtonFramePause = 0;
galtonAnimationContinuous = false;
galtonSoundEnabled = false;
galtonRows = 12;
run([modulepath('random', 'root'), '/examples/galton_board.m']);

assert_istrue(isgraphics(galtonFigure));
assert_istrue(isgraphics(galtonBars));
assert_isequal(numel(galtonBinCounts), galtonRows + 1);
% Balls must have settled into the bins by the end of the run.
assert_istrue(sum(galtonBinCounts) > 0);
% Settled balls are conserved: every count lands in exactly one bin.
assert_isequal(sum(galtonBinCounts), galtonLanded);
delete(galtonFigure);
drawnow();
%=============================================================================
