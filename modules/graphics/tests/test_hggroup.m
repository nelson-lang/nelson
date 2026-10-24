%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
figure();
g = hggroup();
ax = gca();
asserts.istrue(isempty(g.Children));
asserts.isequal(g.DisplayName, '');
asserts.isequal(g.Parent, ax);
asserts.isequal(g.Tag, '');
asserts.isequal(g.Type, 'hggroup');
asserts.istrue(isempty(g.UserData));
asserts.istrue(g.Visible == 'on');
%=============================================================================
figure();
g2 = hggroup();
ln = line(g2, randn(5), randn(5));
g2.Visible = 'off';
asserts.istrue(g2.Visible == 'off');
g2.Visible = 'on';
asserts.istrue(g2.Visible == 'on');
%=============================================================================
figure();
ax = gca();
g1 = hggroup(ax);
asserts.isequal(g1.Parent, ax);
g2 = hggroup(g1);
asserts.isequal(g2.Parent, g1);
g3 = hggroup(g2);
asserts.isequal(g3.Parent, g2);
%=============================================================================
figure();
ax = gca();
g = hggroup();
h = text(0.1, 0.1, 'tttt', 'Parent', g);
asserts.isequal(h.Parent, g);
%=============================================================================
asserts.checkerror('g = hggroup(gcf());', getString(message('Nelson:hggroup:groupCanBeOnly')));
%=============================================================================
figure();
hg = hggroup;
plot(hg, 1:2, 1:2);
plot(hg, 4:5, 4:5);
ax = gca();
asserts.isequal(ax.XLim, [1 5]);
asserts.isequal(ax.YLim, [1 5]);
%=============================================================================
figure();
hg = hggroup();
x = reshape(1:25, 5, 5);
y = reshape(26:50, 5, 5);
p = plot(hg, x, y);
asserts.isequal(size(p), [5 1]);
for k = 1:numel(p)
  asserts.isequal(p(k).Parent, hg);
end
asserts.isequal(numel(hg.Children), 5);
%=============================================================================
figure();
hg = hggroup();
x = reshape(1:25, 5, 5);
y = reshape(26:50, 5, 5);
p = plot(x, y, 'Parent', hg);
asserts.isequal(size(p), [5 1]);
for k = 1:numel(p)
  asserts.isequal(p(k).Parent, hg);
end
asserts.isequal(numel(hg.Children), 5);
%=============================================================================
figure();
ax = gca();
g = hggroup(ax);
R = properties(g);
REF = {'BeingDeleted';
  'BusyAction';
  'Children';
  'CreateFcn';
  'DeleteFcn';
  'DisplayName';
  'Interruptible';
  'Parent';
  'Tag';
  'Type';
  'UserData';
  'Visible'};
asserts.istrue(all(ismember(REF, R)));
%=============================================================================
% Regression: a single object in an hggroup must drive the ancestor axis auto
% limits without a full render (drawnow alone, not getframe). Before the fix the
% axis stayed at its default [0 1] because the group never re-ran the axis state.
f = figure('Visible', 'off');
hg = hggroup;
plot(hg, 1:5, 1:5);
drawnow();
asserts.isequal(gca().XLim, [1 5]);
asserts.isequal(gca().YLim, [1 5]);
close(f);
%=============================================================================
% Regression: a second grouped object must extend the limits (was frozen on the
% first object before the axis-update fix), including via line() with Parent.
f = figure('Visible', 'off');
ax = axes();
hg = hggroup('Parent', ax);
line('Parent', hg, 'XData', 1:2, 'YData', 1:2);
line('Parent', hg, 'XData', 4:5, 'YData', 4:5);
drawnow();
asserts.isequal(ax.XLim, [1 5]);
asserts.isequal(ax.YLim, [1 5]);
close(f);
%=============================================================================
