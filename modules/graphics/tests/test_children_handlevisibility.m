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
% The Children property getter excludes HandleVisibility='off' objects,
% while allchild and findall expose them.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
l1 = __line__('Parent', ax, 'XData', [0 1], 'YData', [0 1]);
l2 = __line__('Parent', ax, 'XData', [0 1], 'YData', [1 0], 'HandleVisibility', 'off');
%=============================================================================
% get(ax, 'Children') excludes the hidden line.
c = get(ax, 'Children');
asserts.isequal(numel(c), 1);
asserts.istrue(any(c == l1));
asserts.isfalse(any(c == l2));
%=============================================================================
% Dot access mirrors the getter.
asserts.isequal(numel(ax.Children), 1);
%=============================================================================
% The no-property struct form also excludes the hidden line.
s = get(ax);
asserts.isequal(numel(s.Children), 1);
%=============================================================================
% allchild returns every child, including the hidden one.
a = allchild(ax);
asserts.isequal(numel(a), 2);
asserts.istrue(any(a == l1));
asserts.istrue(any(a == l2));
%=============================================================================
% findall includes the hidden line, findobj excludes it.
asserts.isequal(numel(findall(ax, 'Type', 'line')), 2);
asserts.isequal(numel(findobj(ax, 'Type', 'line')), 1);
%=============================================================================
fCallback = figure('Visible', 'off');
axOn = axes('Parent', fCallback, 'Tag', 'axisOn', 'HandleVisibility', 'on');
axCallback = axes('Parent', fCallback, 'Tag', 'axisCallback', 'HandleVisibility', 'callback');
lineCallback = __line__('Parent', axCallback, 'XData', 1:3, 'YData', 1:3, 'Tag', 'lineUnderCallback');
figChildren = get(fCallback, 'Children');
asserts.isequal(numel(figChildren), 1);
asserts.isequal(figChildren, axOn);
figChildren = allchild(fCallback);
asserts.istrue(any(figChildren == axCallback));
asserts.istrue(any(figChildren == axOn));
asserts.istrue(isempty(findobj(fCallback, 'Tag', 'axisCallback')));
asserts.isequal(findall(fCallback, 'Tag', 'axisCallback'), axCallback);
asserts.istrue(isempty(findobj(fCallback, 'Tag', 'lineUnderCallback')));
asserts.isequal(findall(fCallback, 'Tag', 'lineUnderCallback'), lineCallback);%=============================================================================
% A legend has HandleVisibility='on', so it stays visible in the figure
% children after being added.
f2 = figure('Visible', 'off');
ax2 = axes('Parent', f2);
plot(ax2, [1 2 3]);
lg = legend(ax2, 'a');
asserts.isequal(get(lg, 'HandleVisibility'), 'on');
figChildren = get(f2, 'Children');
asserts.istrue(any(figChildren == lg));
asserts.istrue(any(figChildren == ax2));
%=============================================================================
close all
%=============================================================================
