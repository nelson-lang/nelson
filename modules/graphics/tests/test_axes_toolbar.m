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
close all
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
tb = ax.Toolbar;
asserts.istrue(isgraphics(tb));
asserts.istrue(ishandle(tb));
asserts.isequal(tb.Type, 'axestoolbar');
asserts.isequal(tb.Parent, ax);
asserts.isequal(char(tb.Visible), 'on');
asserts.isequal(char(tb.HandleVisibility), 'on');
asserts.isequal(tb.BusyAction, 'queue');
asserts.isequal(char(tb.Interruptible), 'on');
asserts.isequal(tb.Children, gobjects(0, 0));
asserts.isequal(tb.ContextMenu, gobjects(0, 0));
asserts.istrue(ismember('SelectionChangedFcn', properties(tb)));
asserts.isfalse(any(ax.Children == tb));
asserts.isfalse(any(findobj(ax) == tb));
asserts.isfalse(any(findall(ax) == tb));
%=============================================================================
ax.Toolbar = [];
asserts.istrue(isempty(ax.Toolbar));
asserts.istrue(isgraphics(tb));
ax.Toolbar = tb;
asserts.isequal(ax.Toolbar, tb);
%=============================================================================
delete(tb);
asserts.isfalse(isgraphics(tb));
asserts.istrue(isempty(ax.Toolbar));
reset(ax);
tb2 = ax.Toolbar;
asserts.istrue(isgraphics(tb2));
asserts.isequal(tb2.Type, 'axestoolbar');
asserts.isequal(tb2.Parent, ax);
%=============================================================================
ax.Toolbar = [];
tb3 = ax.Toolbar;
asserts.istrue(isempty(tb3));
close(f);
asserts.isfalse(isgraphics(tb2));
%=============================================================================
