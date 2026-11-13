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
% axes(container) creates new axes in a figure, panel or tab
f = figure('Visible', 'off');
ax1 = axes(f);
asserts.isequal(ax1.Parent, f);
asserts.isequal(f.CurrentAxes, ax1);
%=============================================================================
p = uipanel(f);
ax2 = axes(p);
asserts.isequal(ax2.Parent, p);
asserts.isequal(f.CurrentAxes, ax2);
asserts.isequal(gca(), ax2);
%=============================================================================
tg = uitabgroup(f);
tb = uitab(tg);
ax3 = axes(tb);
asserts.isequal(ax3.Parent, tb);
asserts.isequal(numel(tb.Children), 1);
asserts.isequal(f.CurrentAxes, ax3);
%=============================================================================
% without output, axes(container) still creates a new current axes
n0 = numel(p.Children);
axes(p);
asserts.isequal(numel(p.Children), n0 + 1);
asserts.isequal(f.CurrentAxes.Parent, p);
assert_isfalse(isequal(f.CurrentAxes, ax2));
%=============================================================================
% axes(existingAxes) makes it current and raises it among its parent children
axes(ax2);
asserts.isequal(f.CurrentAxes, ax2);
asserts.isequal(p.Children(1), ax2);
axes(ax1);
asserts.isequal(f.CurrentAxes, ax1);
% UI containers stay above axes in the figure children
asserts.isequal(f.Children(1), tg);
asserts.isequal(f.Children(2), p);
asserts.isequal(f.Children(3), ax1);
ax4 = axes(f);
asserts.isequal(f.Children(3), ax4);
asserts.isequal(f.Children(4), ax1);
%=============================================================================
% invalid single parents
assert_checkerror('axes(tg)', _('Axes cannot be a child of TabGroup.'));
l = line(ax1, 1:2, 1:2);
assert_checkerror('axes(l)', _('Parent must be a figure or UI container graphics object.'));
%=============================================================================
delete(f);
%=============================================================================
