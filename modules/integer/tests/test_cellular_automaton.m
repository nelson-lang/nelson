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
caFigureVisible = 'off';
caAnimationEnabled = true;
caAnimationFrames = 30;
caFramePause = 0;
caAnimationContinuous = false;
caWidth = 101;
caHeight = 60;
caRule = uint8(90);
run([modulepath('integer', 'root'), '/examples/cellular_automaton.m']);

assert_istrue(isgraphics(caFigure));
assert_istrue(isgraphics(caImage));
assert_isequal(class(caRow), 'uint8');
assert_isequal(size(caBuffer), [caHeight caWidth]);
% The rule table has one uint8 entry per 3-cell neighbourhood.
assert_isequal(numel(caRuleTable), 8);
assert_isequal(class(caRuleTable), 'uint8');
% Rule 90 keeps cells alive, so the lattice is not entirely blank.
assert_istrue(sum(double(caBuffer(:))) > 0);
delete(caFigure);
drawnow();
%=============================================================================
