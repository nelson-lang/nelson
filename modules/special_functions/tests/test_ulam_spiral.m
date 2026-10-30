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
ulamFigureVisible = 'off';
ulamAnimationEnabled = true;
ulamAnimationFrames = 6;
ulamFramePause = 0;
ulamAnimationContinuous = false;
ulamCount = 2601;
run([modulepath('special_functions', 'root'), '/examples/ulam_spiral.m']);

assert_istrue(isgraphics(ulamFigure));
assert_istrue(isgraphics(ulamScatter));
assert_isequal(numel(ulamX), ulamCount);
assert_isequal(numel(ulamY), ulamCount);
% The spiral never revisits a cell, so all coordinates are distinct.
assert_isequal(size(unique([ulamX(:) ulamY(:)], 'rows'), 1), ulamCount);
% primes() marks the known number of primes up to ulamCount.
assert_isequal(numel(ulamPrimeIndices), numel(primes(ulamCount)));
delete(ulamFigure);
drawnow();
%=============================================================================
