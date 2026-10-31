%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% PositionConstraint selects which position drives the layout. A direct set
% maps onto PositionMode: 'innerposition' pins Position (manual), 'outerposition'
% lets the outer box drive it (auto). Setting Position / OuterPosition keeps
% tracking the constraint as before.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
%=============================================================================
% defaults
assert_isequal(char(ax.PositionConstraint), 'outerposition');
assert_isequal(char(ax.PositionMode), 'auto');
%=============================================================================
% direct set 'innerposition' pins the position (PositionMode manual)
ax.PositionConstraint = 'innerposition';
assert_isequal(char(ax.PositionMode), 'manual');
%=============================================================================
% direct set 'outerposition' lets the outer box drive it (PositionMode auto)
ax.PositionConstraint = 'outerposition';
assert_isequal(char(ax.PositionMode), 'auto');
%=============================================================================
% setting Position still flips the constraint to 'innerposition'
ax.Position = [0.2 0.2 0.5 0.5];
assert_isequal(char(ax.PositionConstraint), 'innerposition');
%=============================================================================
% setting OuterPosition flips it back to 'outerposition'
ax.OuterPosition = [0.1 0.1 0.8 0.8];
assert_isequal(char(ax.PositionConstraint), 'outerposition');
%=============================================================================
close(f);
%=============================================================================
