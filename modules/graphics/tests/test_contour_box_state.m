%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
% A contour laid on a freshly replaced axes closes the frame around it: every
% function of the family turns Box on. The rule is tied to NextPlot, not to the
% drawing: an axes being added to, or having only its children replaced, keeps
% the Box it already had. The functions that lay a contour under a surface do
% so while holding the axes, so they leave Box alone. Every expected value
% below was measured.
%=============================================================================
Z = peaks(20);
%=============================================================================
% NextPlot 'replace', the default of a new axes: the frame is closed.
f = figure('Visible', 'off');
ax = axes('Parent', f);
[~, h] = contourf(ax, Z);
asserts.isequal(get(ax, 'NextPlot'), 'replace');
asserts.isequal(char(get(ax, 'Box')), 'on');
asserts.isequal(get(h, 'Type'), 'contour');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
[~, h] = contour(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'on');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
[~, h] = contour3(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'on');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fcontour(ax, @(x, y) x .^ 2 + y .^ 2);
asserts.isequal(char(get(ax, 'Box')), 'on');
close(f);
%=============================================================================
% The axes created on demand answers the same way.
f = figure('Visible', 'off');
[~, h] = contourf(Z);
ax = ancestor(h, 'axes');
asserts.isequal(char(get(ax, 'Box')), 'on');
close(f);
%=============================================================================
% A Box turned off by hand does not survive the replacement.
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'Box', 'off');
[~, h] = contourf(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'on');
close(f);
%=============================================================================
% NextPlot 'add': the contour joins what is already there and the frame is
% left as the caller set it.
f = figure('Visible', 'off');
ax = axes('Parent', f);
hold(ax, 'on');
asserts.isequal(get(ax, 'NextPlot'), 'add');
asserts.isequal(char(get(ax, 'Box')), 'off');
[~, h] = contourf(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
hold(ax, 'on');
[~, h] = contour(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'Box', 'on');
hold(ax, 'on');
[~, h] = contourf(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'on');
close(f);
%=============================================================================
% A plot that closes the frame, then a contour added on top of it: the frame
% stays closed.
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:10);
hold(ax, 'on');
[~, h] = contourf(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'on');
close(f);
%=============================================================================
% NextPlot 'replacechildren': the children go, the axes properties stay.
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'NextPlot', 'replacechildren');
[~, h] = contourf(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'NextPlot', 'replacechildren');
[~, h] = contour(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
set(ax, 'Box', 'on');
set(ax, 'NextPlot', 'replacechildren');
[~, h] = contourf(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'on');
close(f);
%=============================================================================
% A contour laid under a surface is drawn while the axes is held, so the frame
% is left open there.
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = surfc(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'off');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = meshc(ax, Z);
asserts.isequal(char(get(ax, 'Box')), 'off');
close(f);
%=============================================================================
