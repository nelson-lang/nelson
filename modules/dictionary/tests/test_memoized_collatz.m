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
collatzFigureVisible = 'off';
collatzAnimationEnabled = true;
collatzFramePause = 0;
collatzAnimationContinuous = false;
collatzMax = 400;
run([modulepath('dictionary', 'root'), '/examples/memoized_collatz.m']);

assert_istrue(isgraphics(collatzFigure));
assert_istrue(isgraphics(collatzScatter));
% The dictionary memoized every starting number plus its orbit intermediates.
assert_istrue(isKey(collatzCache, 6));
assert_istrue(numEntries(collatzCache) > collatzMax);
% Collatz(6): 6->3->10->5->16->8->4->2->1 is eight steps.
assert_isequal(collatzCache(6), 8);
assert_isequal(collatzPointsY(6), 8);
delete(collatzFigure);
drawnow();
%=============================================================================
