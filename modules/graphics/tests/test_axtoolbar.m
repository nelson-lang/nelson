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
tb = axtoolbar();
asserts.isequal(tb, ax.Toolbar);
asserts.isequal(tb.Type, 'axestoolbar');
asserts.isequal(char(tb.Visible), 'on');
%=============================================================================
axtoolbar('visible', 'off');
asserts.isequal(char(ax.Toolbar.Visible), 'off');
axtoolbar(ax, 'Visible', 'on');
asserts.isequal(char(ax.Toolbar.Visible), 'on');
%=============================================================================
[tb, btns] = axtoolbar(ax, {'zoomin', 'pan'}, 'Tag', 'moon');
asserts.isequal(tb, ax.Toolbar);
asserts.isequal(tb.Tag, 'moon');
asserts.isequal(btns, gobjects(0, 0));
tb = axtoolbar('default');
asserts.isequal(tb, ax.Toolbar);
tb = axtoolbar(ax, ["export", "restoreview"]);
asserts.isequal(tb, ax.Toolbar);
%=============================================================================
ax2 = axes('Parent', f);
tb2 = axtoolbar(ax2, 'Visible', 'off');
asserts.isequal(tb2, ax2.Toolbar);
asserts.isequal(char(ax2.Toolbar.Visible), 'off');
asserts.isequal(char(ax.Toolbar.Visible), 'on');
%=============================================================================
id = '';
try
  axtoolbar(ax, 42);
catch ex
  id = ex.identifier;
end
asserts.isequal(id, 'Nelson:graphics:invalidToolbarButtons');
id = '';
try
  axtoolbar(ax, {'pan', 'fly'});
catch ex
  id = ex.identifier;
end
asserts.isequal(id, 'Nelson:graphics:invalidToolbarButtons');
%=============================================================================
ax.Toolbar = [];
id = '';
try
  axtoolbar(ax);
catch ex
  id = ex.identifier;
end
asserts.isequal(id, 'Nelson:graphics:noAxesToolbar');
reset(ax);
asserts.isequal(axtoolbar(ax), ax.Toolbar);
%=============================================================================
close(f);
%=============================================================================
