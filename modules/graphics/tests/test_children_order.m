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
% A container that stacks its children reports them top first: the object added
% last is drawn over the others and heads the list. Containers that lay their
% children out side by side report them in the order they were added.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
l1 = line('XData', [0 1], 'YData', [0 1]);
l2 = line('XData', [0 1], 'YData', [1 0]);
l3 = line('XData', [0 1], 'YData', [0 0]);
c = get(ax, 'Children');
asserts.isequal(numel(c), 3);
asserts.istrue(isequal(c(1), l3));
asserts.istrue(isequal(c(2), l2));
asserts.istrue(isequal(c(3), l1));
%=============================================================================
% Dot access and the no-property struct form report the same order.
asserts.istrue(isequal(ax.Children, c));
s = get(ax);
asserts.istrue(isequal(s.Children, c));
%=============================================================================
% allchild, findobj and findall all follow the Children order.
asserts.istrue(isequal(allchild(ax), c));
asserts.istrue(isequal(findobj(ax, 'Type', 'line'), c));
asserts.istrue(isequal(findall(ax, 'Type', 'line'), c));
%=============================================================================
% findobj lists the axes itself before descending into its children.
all = findobj(ax);
asserts.isequal(numel(all), 4);
asserts.istrue(isequal(all(1), ax));
asserts.istrue(isequal(all(2:end), c));
%=============================================================================
close all
%=============================================================================
% Assigning Children is a reordering: what comes back out is what went in.
f = figure('Visible', 'off');
ax = axes('Parent', f);
b1 = line('XData', [0 1], 'YData', [0 1], 'Parent', ax);
b2 = line('XData', [0 1], 'YData', [1 0], 'Parent', ax);
b3 = line('XData', [0 1], 'YData', [0 0], 'Parent', ax);
set(ax, 'Children', [b1; b2; b3]);
asserts.istrue(isequal(get(ax, 'Children'), [b1; b2; b3]));
set(ax, 'Children', [b3; b1; b2]);
asserts.istrue(isequal(get(ax, 'Children'), [b3; b1; b2]));
%=============================================================================
close all
%=============================================================================
% The head of Children is the object drawn on top. Two overlapping opaque
% patches: the newest covers the oldest, and moving the oldest to the head of
% Children brings it in front. The figure stays hidden so that the check reads
% the same drawing path the tests run everywhere else.
f = figure('Visible', 'off', 'Color', 'w');
ax = axes('Parent', f, 'Position', [0 0 1 1], 'XLim', [0 10], 'YLim', [0 10]);
axis(ax, 'off');
red = patch('XData', [0 10 10 0], 'YData', [0 0 10 10], 'FaceColor', [1 0 0], ...
  'EdgeColor', 'none', 'Parent', ax);
blue = patch('XData', [2 8 8 2], 'YData', [2 2 8 8], 'FaceColor', [0 0 1], ...
  'EdgeColor', 'none', 'Parent', ax);
drawnow();
img = getframe(f).cdata;
centre = double(reshape(img(round(size(img, 1) / 2), round(size(img, 2) / 2), :), 1, []));
asserts.isequal(centre, [0 0 255]);
asserts.istrue(isequal(get(ax, 'Children'), [blue; red]));
%=============================================================================
set(ax, 'Children', [red; blue]);
drawnow();
img = getframe(f).cdata;
centre = double(reshape(img(round(size(img, 1) / 2), round(size(img, 2) / 2), :), 1, []));
asserts.isequal(centre, [255 0 0]);
%=============================================================================
close all
%=============================================================================
% A figure reports its axes the same way, and a group reports its own children.
f = figure('Visible', 'off');
a1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
a2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
figChildren = get(f, 'Children');
asserts.istrue(isequal(figChildren(1), a2));
asserts.istrue(isequal(figChildren(2), a1));
%=============================================================================
g = hggroup('Parent', a1);
n1 = line('XData', [0 1], 'YData', [0 1], 'Parent', g);
n2 = line('XData', [0 1], 'YData', [1 0], 'Parent', g);
groupChildren = get(g, 'Children');
asserts.istrue(isequal(groupChildren(1), n2));
asserts.istrue(isequal(groupChildren(2), n1));
%=============================================================================
close all
%=============================================================================
% Moving an object into another axes puts it on top there.
f = figure('Visible', 'off');
ax1 = axes('Parent', f);
moved = line('XData', [0 1], 'YData', [0 1], 'Parent', ax1, 'Tag', 'moved');
f2 = figure('Visible', 'off');
ax2 = axes('Parent', f2);
line('XData', [0 1], 'YData', [0 0], 'Parent', ax2, 'Tag', 'stays');
set(moved, 'Parent', ax2);
tags = arrayfun(@(h) get(h, 'Tag'), get(ax2, 'Children'), 'UniformOutput', false);
asserts.isequal(tags, {'moved'; 'stays'});
%=============================================================================
close all
%=============================================================================
% copyobj hands back the copies in the order it was given, and the destination
% reports them that way too.
f = figure('Visible', 'off');
ax = axes('Parent', f);
k1 = line('XData', [0 1], 'YData', [0 1], 'Parent', ax, 'Tag', 'k1');
k2 = line('XData', [0 1], 'YData', [1 0], 'Parent', ax, 'Tag', 'k2');
f2 = figure('Visible', 'off');
ax2 = axes('Parent', f2);
copies = copyobj(get(ax, 'Children'), ax2);
copiedTags = arrayfun(@(h) get(h, 'Tag'), copies, 'UniformOutput', false);
asserts.isequal(copiedTags, {'k2'; 'k1'});
destTags = arrayfun(@(h) get(h, 'Tag'), get(ax2, 'Children'), 'UniformOutput', false);
asserts.isequal(destTags, {'k2'; 'k1'});
%=============================================================================
close all
%=============================================================================
% Saving and reopening a figure keeps the order.
f = figure('Visible', 'off');
ax = axes('Parent', f);
line('XData', [0 1], 'YData', [0 1], 'Parent', ax, 'Tag', 'm1');
line('XData', [0 1], 'YData', [1 0], 'Parent', ax, 'Tag', 'm2');
line('XData', [0 1], 'YData', [0 0], 'Parent', ax, 'Tag', 'm3');
savedTags = arrayfun(@(h) get(h, 'Tag'), get(ax, 'Children'), 'UniformOutput', false);
asserts.isequal(savedTags, {'m3'; 'm2'; 'm1'});
figFile = [tempname(), '.fig'];
savefig(f, figFile);
close(f);
reopened = openfig(figFile, 'invisible');
reopenedAxes = findobj(reopened, 'Type', 'axes');
reopenedTags = arrayfun(@(h) get(h, 'Tag'), get(reopenedAxes(1), 'Children'), 'UniformOutput', false);
asserts.isequal(reopenedTags, {'m3'; 'm2'; 'm1'});
close(reopened);
rmfile(figFile);
%=============================================================================
close all
%=============================================================================
% A tab group and a tree lay their children out in sequence, so they keep the
% order the children were added in.
f = uifigure('Visible', 'off');
tabs = uitabgroup(f);
uitab(tabs, 'Title', 't1');
uitab(tabs, 'Title', 't2');
tabTitles = arrayfun(@(h) get(h, 'Title'), get(tabs, 'Children'), 'UniformOutput', false);
asserts.isequal(tabTitles, {'t1'; 't2'});
%=============================================================================
tree = uitree(f);
uitreenode(tree, 'Text', 'n1');
uitreenode(tree, 'Text', 'n2');
nodeTexts = arrayfun(@(h) get(h, 'Text'), get(tree, 'Children'), 'UniformOutput', false);
asserts.isequal(nodeTexts, {'n1'; 'n2'});
%=============================================================================
% A panel stacks its children, so it reports them top first.
panel = uipanel(f);
p1 = uibutton(panel, 'Text', 'p1');
p2 = uibutton(panel, 'Text', 'p2');
panelChildren = get(panel, 'Children');
asserts.istrue(isequal(panelChildren(1), p2));
asserts.istrue(isequal(panelChildren(2), p1));
%=============================================================================
close all
%=============================================================================
