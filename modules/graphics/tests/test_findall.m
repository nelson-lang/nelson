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
asserts.istrue(isempty(findall(0, 'Tag', 'ti92App')));
asserts.istrue(isempty(findall(groot(), 'Tag', 'ti92App')));
%=============================================================================
f = figure('Visible', 'off', 'Tag', 'ti92App');
ax = axes('Parent', f, 'Tag', 'axesTag');
ln = plot(ax, 1:3, 'Tag', 'lineTag');
%=============================================================================
asserts.isequal(findall(0, 'Tag', 'ti92App'), f);
asserts.isequal(findall(groot(), 'Tag', 'ti92App'), f);
asserts.isequal(findall(0, 'Type', 'line'), ln);
asserts.isequal(findall(f, 'Tag', 'axesTag'), ax);
asserts.isequal(findall(f, 'Tag', 'lineTag'), ln);
%=============================================================================
ax.HandleVisibility = 'off';
asserts.isequal(findall(0, 'Tag', 'axesTag'), ax);
%=============================================================================
root = groot();
previous = root.ShowHiddenHandles;
rootCleanup = onCleanup(@() set(root, 'ShowHiddenHandles', previous));
root.ShowHiddenHandles = 'off';
f.HandleVisibility = 'off';
asserts.istrue(isempty(root.Children));
asserts.istrue(isempty(findobj(root, 'Tag', 'ti92App')));
asserts.isequal(findall(root, 'Tag', 'ti92App'), f);
asserts.isequal(findall(root, 'Tag', 'lineTag'), ln);
asserts.isequal(allchild(root), f);
asserts.isequal(root.ShowHiddenHandles, 'off');
clear rootCleanup;
%=============================================================================
close(f);
%=============================================================================
g = groot();
g.ShowHiddenHandles = 'off';
f1 = figure('Visible', 'off', 'HandleVisibility', 'on', 'Tag', 'rootHiddenF1');
ax1 = axes('Parent', f1, 'Tag', 'rootHiddenAx1');
__line__('Parent', ax1, 'XData', 1:3, 'YData', 1:3, 'Tag', 'rootHiddenLine1');
f2 = figure('Visible', 'off', 'HandleVisibility', 'off', 'Tag', 'rootHiddenF2');
ax2 = axes('Parent', f2, 'Tag', 'rootHiddenAx2');
ln2 = __line__('Parent', ax2, 'XData', 1:3, 'YData', 3:-1:1, 'Tag', 'rootHiddenLine2');
%=============================================================================
children = g.Children;
asserts.isequal(numel(children), 1);
asserts.isequal(children, f1);
children = allchild(g);
asserts.isequal(numel(children), 2);
asserts.isequal(children(1), f2);
asserts.isequal(children(2), f1);
asserts.istrue(isempty(findobj(g, 'Tag', 'rootHiddenF2')));
asserts.isequal(findall(g, 'Tag', 'rootHiddenF2'), f2);
asserts.isequal(findall(g, 'Tag', 'rootHiddenAx2'), ax2);
asserts.isequal(findall(g, 'Tag', 'rootHiddenLine2'), ln2);
asserts.istrue(isempty(findall(g, '-depth', 0, 'Tag', 'rootHiddenF2')));
%=============================================================================
g.ShowHiddenHandles = 'on';
children = g.Children;
asserts.isequal(numel(children), 2);
asserts.isequal(children(1), f2);
asserts.isequal(children(2), f1);
children = allchild(g);
asserts.isequal(numel(children), 2);
asserts.isequal(children(1), f2);
asserts.isequal(children(2), f1);
asserts.isequal(findobj(g, 'Tag', 'rootHiddenF2'), f2);
%=============================================================================
f2.HandleVisibility = 'callback';
g.ShowHiddenHandles = 'off';
children = g.Children;
asserts.isequal(numel(children), 1);
asserts.isequal(children, f1);
children = allchild(g);
asserts.isequal(numel(children), 2);
asserts.isequal(children(1), f2);
asserts.isequal(children(2), f1);
asserts.istrue(isempty(findobj(g, 'Tag', 'rootHiddenF2')));
asserts.isequal(findall(g, 'Tag', 'rootHiddenF2'), f2);
%=============================================================================
close all
reset(g);
%=============================================================================
