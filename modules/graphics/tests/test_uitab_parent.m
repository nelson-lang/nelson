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
close all force
msg = _('Parent must be a TabGroup object.');
id = 'Nelson:uicomponent:parentMustBeTabGroupObject';
%=============================================================================
% parents other than a tab group are rejected
%=============================================================================
fig = figure('Visible', 'off');
ufig = uifigure('Visible', 'off');
parents = {fig, ufig, uipanel(fig), uipanel(ufig), uigridlayout(ufig), uitab(uitabgroup(fig))};
for k = 1:numel(parents)
  p = parents{k};
  before = numel(p.Children);
  assert_checkerror('uitab(p)', msg, id);
  assert_checkerror('uitab(p, ''Title'', ''T'')', msg, id);
  assert_checkerror('uitab(''Parent'', p)', msg, id);
  assert_isequal(numel(p.Children), before);
  t = uitab(uitabgroup(fig));
  assert_checkerror('set(t, ''Parent'', p)', msg, id);
  assert_checkerror('setParent(t, p)', msg, id);
end
%=============================================================================
% a tab group parent works in all forms
%=============================================================================
tg = uitabgroup(ufig);
t1 = uitab(tg);
t2 = uitab('Parent', tg, 'Title', 'B');
assert_isequal(t1.Parent, tg);
assert_isequal(t2.Parent, tg);
assert_isequal(t2.Title, 'B');
tg2 = uitabgroup(fig);
t1.Parent = tg2;
assert_isequal(t1.Parent, tg2);
close all force
%=============================================================================
% no parent: a figure and a tab group are created, then reused
%=============================================================================
t = uitab();
tg = t.Parent;
assert_isequal(class(tg), 'nelson.ui.container.TabGroup');
f = tg.Parent;
assert_isequal(f, gcf());
assert_isequal(tg.Units, 'normalized');
assert_isequal(tg.Position, [0 0 1 1]);
t2 = uitab('Title', 'Second');
assert_isequal(t2.Parent, tg);
assert_isequal(numel(findall(0, 'type', 'figure')), 1);
close all force
%=============================================================================
% no parent: the last tab group of the current figure is used
%=============================================================================
f = figure('Visible', 'off');
tg1 = uitabgroup(f);
tg2 = uitabgroup(f);
t = uitab();
assert_isequal(t.Parent, tg2);
close all force
%=============================================================================
% no parent: hidden tab groups, nested tab groups and uifigures are skipped
%=============================================================================
f = figure('Visible', 'off');
hidden = uitabgroup(f, 'HandleVisibility', 'off');
nested = uitabgroup(uipanel(f));
t = uitab();
assert_isequal(t.Parent.Parent, f);
assert_isfalse(isequal(t.Parent, hidden));
assert_isfalse(isequal(t.Parent, nested));
close all force
ufig = uifigure('Visible', 'off');
t = uitab();
assert_isfalse(isequal(t.Parent.Parent, ufig));
close all force
%=============================================================================
function setParent(t, p)
  t.Parent = p;
end
%=============================================================================
