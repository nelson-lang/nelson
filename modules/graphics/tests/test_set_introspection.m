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
% One-argument set(h) returns a struct of the settable properties, each value
% being the cell array of allowed enumerated choices (empty for free-form ones).
f = figure('Visible', 'off');
h = plot(1:10);
%=============================================================================
s = set(h);
assert_istrue(isstruct(s));
assert_istrue(isscalar(s));
%=============================================================================
% Enumerated properties expose their allowed choices as a cell array.
assert_istrue(isfield(s, 'LineStyle'));
assert_isequal(s.LineStyle, {'-'; '--'; ':'; '-.'; 'none'});
assert_isequal(s.Visible, {'on'; 'off'});
assert_isequal(s.HandleVisibility, {'on'; 'callback'; 'off'});
%=============================================================================
% Free-form properties come back as an empty cell.
assert_istrue(isfield(s, 'LineWidth'));
assert_istrue(iscell(s.LineWidth));
assert_istrue(isempty(s.LineWidth));
%=============================================================================
% Read-only properties are excluded from the settable struct.
assert_isfalse(isfield(s, 'Type'));
%=============================================================================
% Two-argument set(h, 'Prop') returns the cell array of allowed choices.
c = set(h, 'LineStyle');
assert_isequal(c, {'-'; '--'; ':'; '-.'; 'none'});
assert_isequal(set(h, 'Visible'), {'on'; 'off'});
%=============================================================================
% A free-form property returns an empty cell array.
lw = set(h, 'LineWidth');
assert_istrue(iscell(lw));
assert_istrue(isempty(lw));
%=============================================================================
% Case-insensitive property name resolution, as get() does.
assert_isequal(set(h, 'linestyle'), {'-'; '--'; ':'; '-.'; 'none'});
%=============================================================================
% An unknown property is rejected.
assert_checkerror('set(h, ''NotAProperty'')', getString(message('Nelson:graphics:unrecognizedProperty', 'NotAProperty')), 'Nelson:graphics:unrecognizedProperty');
%=============================================================================
close(f);
%=============================================================================
% The introspection form also works on a figure handle.
f = figure('Visible', 'off');
sf = set(f);
assert_istrue(isstruct(sf));
assert_istrue(isfield(sf, 'Visible'));
assert_isequal(sf.Visible, {'on'; 'off'});
close(f);
%=============================================================================
