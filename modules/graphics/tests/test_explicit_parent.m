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
% A constructor given an explicit 'Parent' builds into that parent, even when
% the current axes is another one.
%=============================================================================
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
asserts.istrue(isequal(gca(), ax2));
%=============================================================================
t = text('Parent', ax1, 'Position', [0.5 0.5 0], 'String', 'x');
l = __line__('Parent', ax1, 'XData', [0 1], 'YData', [0 1]);
p = patch('Parent', ax1, 'XData', [0 1 1]', 'YData', [0 0 1]');
im = image('Parent', ax1, 'CData', magic(4));
g = hggroup('Parent', ax1);
%=============================================================================
% None of these low-level forms clears the axes, so all five are still there.
children = get(ax1, 'Children');
built = [t, l, p, im, g];
for k = 1:numel(built)
  asserts.istrue(isequal(get(built(k), 'Parent'), ax1));
  asserts.istrue(any(children == built(k)));
end
asserts.isequal(numel(get(ax2, 'Children')), 0);
%=============================================================================
% Naming a parent does not make it the current axes.
asserts.istrue(isequal(gca(), ax2));
%=============================================================================
% The leading axes argument names the parent just as well. Checked one at a
% time: the high-level forms go through newplot() and may clear the axes,
% which would invalidate the handles built before them.
%=============================================================================
t2 = text(ax1, 0.5, 0.5, 'y');
asserts.istrue(isequal(get(t2, 'Parent'), ax1));
asserts.istrue(any(get(ax1, 'Children') == t2));

l2 = line(ax1, [0 1], [1 0]);
asserts.istrue(isequal(get(l2, 'Parent'), ax1));
asserts.istrue(any(get(ax1, 'Children') == l2));

p2 = patch(ax1, [0 1 1], [1 0 1], 'red');
asserts.istrue(isequal(get(p2, 'Parent'), ax1));
asserts.istrue(any(get(ax1, 'Children') == p2));

p3 = patch(ax1, 'Faces', [1 2 3], 'Vertices', [0 0 0; 1 0 0; 1 1 0]);
asserts.istrue(isequal(get(p3, 'Parent'), ax1));
asserts.istrue(any(get(ax1, 'Children') == p3));

im2 = image(ax1, magic(3));
asserts.istrue(isequal(get(im2, 'Parent'), ax1));
asserts.istrue(any(get(ax1, 'Children') == im2));

g2 = hggroup(ax1);
asserts.istrue(isequal(get(g2, 'Parent'), ax1));
asserts.istrue(any(get(ax1, 'Children') == g2));
%=============================================================================
% The other axes never received any of them, and stays the current one.
asserts.isequal(numel(get(ax2, 'Children')), 0);
asserts.istrue(isequal(gca(), ax2));
%=============================================================================
% The property name is resolved without regard to case, so a lowercase
% 'parent' is explicit too.
%=============================================================================
l3 = __line__('parent', ax1, 'XData', [0 1], 'YData', [0 0.5]);
asserts.istrue(isequal(get(l3, 'Parent'), ax1));
asserts.istrue(any(get(ax1, 'Children') == l3));
asserts.isequal(numel(get(ax2, 'Children')), 0);
%=============================================================================
% A group parent keeps working, and the object reaches the axes through it.
%=============================================================================
lg = __line__('Parent', g2, 'XData', [0 1], 'YData', [0 1]);
asserts.istrue(isequal(get(lg, 'Parent'), g2));
asserts.istrue(any(get(g2, 'Children') == lg));
asserts.isfalse(any(get(ax1, 'Children') == lg));
asserts.istrue(isequal(ancestor(lg, 'axes'), ax1));
%=============================================================================
% An axes in another figure is reached the same way, and the current figure
% is left untouched.
%=============================================================================
f2 = figure('Visible', 'off');
ax3 = axes('Parent', f2);
asserts.istrue(isequal(gca(), ax3));
lf = __line__('Parent', ax1, 'XData', [0 1], 'YData', [0 2]);
asserts.istrue(isequal(get(lf, 'Parent'), ax1));
asserts.istrue(any(get(ax1, 'Children') == lf));
asserts.isequal(numel(get(ax3, 'Children')), 0);
asserts.istrue(isequal(gca(), ax3));
close(f2);
%=============================================================================
% 'AutoParent', 'off' still attaches nothing: the label objects live in the
% Title/XLabel properties of the axes, not in its children.
%=============================================================================
figure(f);
axes(ax2);
before = numel(get(ax1, 'Children'));
title(ax1, 'a title');
xlabel(ax1, 'a label');
asserts.isequal(get(get(ax1, 'Title'), 'String'), 'a title');
asserts.isequal(get(get(ax1, 'XLabel'), 'String'), 'a label');
asserts.istrue(isequal(get(get(ax1, 'Title'), 'Parent'), ax1));
asserts.isequal(numel(get(ax1, 'Children')), before);
%=============================================================================
% An unusable parent is still reported against the 'Parent' property: a figure
% does not resolve as a parent at all, and a container frames an axes rather
% than holding a graphics object of its own.
%=============================================================================
assert_checkerror('__line__(''Parent'', f, ''XData'', [0 1], ''YData'', [0 1])', graphicsPropertyErrorMessage('Parent', 'Nelson:graphics:invalidGraphicsObject'), ...
  'Nelson:graphics:propertyError');
%=============================================================================
panel = uipanel('Parent', f);
button = uicontrol('Parent', f, 'Style', 'pushbutton');
containers = {panel, button};
for k = 1:numel(containers)
  assert_checkerror('__line__(''Parent'', containers{k}, ''XData'', [0 1], ''YData'', [0 1])', graphicsPropertyErrorMessage('Parent', 'Nelson:graphics:parentIsNotAxesOrGroup'), ...
    'Nelson:graphics:propertyError');
  assert_checkerror('__text__(''Parent'', containers{k}, ''String'', ''x'')', graphicsPropertyErrorMessage('Parent', 'Nelson:graphics:parentIsNotAxesOrGroup'), ...
    'Nelson:graphics:propertyError');
end
%=============================================================================
% An axes inside a container is a perfectly good parent, though.
%=============================================================================
axPanel = axes('Parent', panel);
axes(ax2);
lp = __line__('Parent', axPanel, 'XData', [0 1], 'YData', [0 1]);
asserts.istrue(isequal(get(lp, 'Parent'), axPanel));
asserts.istrue(any(get(axPanel, 'Children') == lp));
asserts.istrue(isequal(gca(), ax2));
%=============================================================================
close all
%=============================================================================
