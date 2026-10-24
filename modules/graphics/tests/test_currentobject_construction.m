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
% CurrentObject reports what the user last clicked in a figure. Building a
% graphics object is not a click: every constructor must leave CurrentObject
% exactly as it found it.
%=============================================================================
close all
%=============================================================================
% a fresh figure has no current object, and neither axes nor lines pick one
f = figure('Visible', 'off');
asserts.istrue(isempty(f.CurrentObject));
ax = axes('Parent', f);
asserts.istrue(isempty(f.CurrentObject));
l1 = line('XData', [0 1], 'YData', [0 1]);
asserts.istrue(isempty(f.CurrentObject));
l2 = line('XData', [0 1], 'YData', [1 0]);
l3 = line('XData', [0 1], 'YData', [0 0]);
asserts.istrue(isempty(f.CurrentObject));
asserts.istrue(isempty(gco()));
%=============================================================================
% the other primitives behave the same
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
patch('XData', [0 1 1], 'YData', [0 0 1]);
asserts.istrue(isempty(f.CurrentObject));
text(0.5, 0.5, 'hello');
asserts.istrue(isempty(f.CurrentObject));
image([1 2; 3 4]);
asserts.istrue(isempty(f.CurrentObject));
surface(peaks(5));
asserts.istrue(isempty(f.CurrentObject));
hggroup();
asserts.istrue(isempty(f.CurrentObject));
%=============================================================================
% high level plotting builds through the same constructors
close all
f = figure('Visible', 'off');
plot(1:3, 1:3);
asserts.istrue(isempty(f.CurrentObject));
close all
f = figure('Visible', 'off');
scatter(1:3, 1:3);
asserts.istrue(isempty(f.CurrentObject));
close all
f = figure('Visible', 'off');
bar(1:3);
asserts.istrue(isempty(f.CurrentObject));
close all
f = figure('Visible', 'off');
surf(peaks(5));
asserts.istrue(isempty(f.CurrentObject));
%=============================================================================
% decorations attached to an existing plot
close all
f = figure('Visible', 'off');
plot(1:3);
legend('a');
asserts.istrue(isempty(f.CurrentObject));
close all
f = figure('Visible', 'off');
surf(peaks(5));
colorbar();
asserts.istrue(isempty(f.CurrentObject));
close all
f = figure('Visible', 'off');
annotation('arrow', [0.2 0.4], [0.2 0.4]);
asserts.istrue(isempty(f.CurrentObject));
%=============================================================================
% user interface objects
close all
f = figure('Visible', 'off');
uicontrol('Parent', f, 'Style', 'pushbutton');
asserts.istrue(isempty(f.CurrentObject));
uimenu('Parent', f, 'Text', 'a menu');
asserts.istrue(isempty(f.CurrentObject));
uicontextmenu('Parent', f);
asserts.istrue(isempty(f.CurrentObject));
uitable('Parent', f);
asserts.istrue(isempty(f.CurrentObject));
p = uipanel('Parent', f);
asserts.istrue(isempty(f.CurrentObject));
tg = uitabgroup('Parent', f);
uitab('Parent', tg);
asserts.istrue(isempty(f.CurrentObject));
% an axes built inside a container is still not a click
axes('Parent', p);
asserts.istrue(isempty(f.CurrentObject));
%=============================================================================
% copyobj duplicates an object without picking it
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
l = line('XData', [0 1], 'YData', [0 1]);
copyobj(l, ax);
asserts.istrue(isempty(f.CurrentObject));
%=============================================================================
% axes(h) promotes an existing axes to current without picking an object
close all
f = figure('Visible', 'off');
a1 = axes('Parent', f);
a2 = axes('Parent', f);
axes(a1);
asserts.isequal(f.CurrentAxes, a1);
asserts.istrue(isempty(f.CurrentObject));
%=============================================================================
% reopening a saved figure restores its children, not a selection
close all
f = figure('Visible', 'off');
axes('Parent', f);
line('XData', [0 1], 'YData', [0 1]);
figFilename = [tempname(), '.fig'];
savefig(f, figFilename);
close all
f2 = openfig(figFilename, 'invisible');
asserts.istrue(isempty(f2.CurrentObject));
rmfile(figFilename);
%=============================================================================
% cla empties the axes without picking anything: a selection outside the axes
% survives it, and one inside it goes away only because the object is deleted
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
btn = uicontrol('Parent', f, 'Style', 'pushbutton');
f.CurrentObject = btn;
cla(ax);
asserts.isequal(f.CurrentObject, btn);
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
l = line('XData', [0 1], 'YData', [0 1]);
f.CurrentObject = l;
cla(ax);
asserts.istrue(isempty(f.CurrentObject));
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
f.CurrentObject = ax;
cla(ax);
asserts.isequal(f.CurrentObject, ax);
%=============================================================================
% clf clears the whole figure, selection included
close all
f = figure('Visible', 'off');
axes('Parent', f);
btn = uicontrol('Parent', f, 'Style', 'pushbutton');
f.CurrentObject = btn;
clf(f);
asserts.istrue(isempty(f.CurrentObject));
%=============================================================================
% an explicit assignment is honoured, and survives later constructions
close all
f = figure('Visible', 'off');
ax = axes('Parent', f);
l1 = line('XData', [0 1], 'YData', [0 1]);
f.CurrentObject = l1;
asserts.isequal(f.CurrentObject, l1);
asserts.isequal(gco(), l1);
l2 = line('XData', [0 1], 'YData', [1 0]);
asserts.isequal(f.CurrentObject, l1);
asserts.isequal(gco(), l1);
axes('Parent', f);
asserts.isequal(f.CurrentObject, l1);
%=============================================================================
close all
%=============================================================================
