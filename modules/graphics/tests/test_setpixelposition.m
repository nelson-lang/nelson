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
f = figure('Visible', 'off', 'Position', [100 100 500 400]);
% the Position is stored in the units of the object, which are kept
c = uicontrol(f, 'Units', 'normalized', 'Position', [0.1 0.1 0.2 0.2]);
setpixelposition(c, [51 81 250 100]);
asserts.isapprox(c.Position, [0.1 0.2 0.5 0.25], 1e-12);
asserts.isequal(c.Units, 'normalized');
asserts.isapprox(getpixelposition(c), [51 81 250 100], 1e-9);
c.Units = 'characters';
setpixelposition(c, [20 30 100 50]);
asserts.isapprox(c.Position, [19 / (28 / 3), 29 / 20, 100 / (28 / 3), 2.5], 1e-9);
asserts.isequal(c.Units, 'characters');
asserts.isapprox(getpixelposition(c), [20 30 100 50], 1e-9);
% a column vector is accepted
c.Units = 'pixels';
setpixelposition(c, [21; 31; 101; 51]);
asserts.isequal(c.Position, [21 31 101 51]);
%=============================================================================
ax = axes(f, 'Units', 'normalized', 'Position', [0.5 0.5 0.3 0.3]);
setpixelposition(ax, [51 81 250 100]);
asserts.isapprox(ax.Position, [0.1 0.2 0.5 0.25], 1e-9);
asserts.isequal(ax.Units, 'normalized');
l = line(ax, [0 1], [0 1]);
%=============================================================================
% recursive: the position is given in the figure
outer = uipanel(f, 'Units', 'pixels', 'Position', [50 60 300 200], 'BorderType', 'none');
child = uicontrol(outer, 'Units', 'pixels', 'Position', [1 1 10 10]);
setpixelposition(child, [70 90 60 20]);
asserts.isequal(child.Position, [70 90 60 20]);
setpixelposition(child, [70 90 60 20], false);
asserts.isequal(child.Position, [70 90 60 20]);
setpixelposition(child, [70 90 60 20], true);
asserts.isequal(child.Position, [20 30 60 20]);
setpixelposition(child, [70 90 60 20], 1);
asserts.isequal(child.Position, [20 30 60 20]);
inner = uipanel(outer, 'Units', 'normalized', 'Position', [0.1 0.1 0.5 0.5]);
setpixelposition(inner, [100 100 150 100], true);
asserts.isapprox(inner.Position, [49 / 300, 39 / 200, 0.5, 0.5], 1e-9);
% the parent of a child of the figure does not shift it
setpixelposition(outer, [40 50 300 200], true);
asserts.isequal(outer.Position, [40 50 300 200]);
%=============================================================================
% a figure, by handle or by number, in any units
setpixelposition(f, [120 130 400 300]);
asserts.isequal(f.Position, [120 130 400 300]);
setpixelposition(f.Number, [100 100 500 400]);
asserts.isequal(f.Position, [100 100 500 400]);
f.Units = 'normalized';
setpixelposition(f, [110 120 500 400]);
asserts.isequal(f.Units, 'normalized');
asserts.isapprox(getpixelposition(f), [110 120 500 400], 1e-6);
f.Units = 'pixels';
%=============================================================================
% an empty first argument does nothing
setpixelposition([], [1 1 10 10]);
%=============================================================================
invalidHandle = getString(message('Nelson:graphics:setpixelposition:invalidHandle'));
invalidPosition = getString(message('Nelson:graphics:setpixelposition:invalidPosition'));
invalidRecursive = getString(message('Nelson:graphics:setpixelposition:invalidRecursive'));
noPosition = getString(message('Nelson:graphics:setpixelposition:noPosition'));
asserts.checkerror('setpixelposition();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('setpixelposition(c);', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('setpixelposition(c, [1 1 10 10], true, 1);', getString(message('Nelson:tooManyInputs')));
asserts.checkerror('x = setpixelposition(c, [1 1 10 10]);', getString(message('Nelson:nargoutchk:tooManyOutputs')));
asserts.checkerror('setpixelposition(c, [1 1 10]);', invalidPosition);
asserts.checkerror('setpixelposition(c, ''abcd'');', invalidPosition);
asserts.checkerror('setpixelposition(c, [1 1 10 10i]);', invalidPosition);
asserts.checkerror('setpixelposition([c, child], [1 1 10 10]);', invalidHandle);
asserts.checkerror('setpixelposition(42.5, [1 1 10 10]);', invalidHandle);
asserts.checkerror('setpixelposition(''a'', [1 1 10 10]);', invalidHandle);
asserts.checkerror('setpixelposition(c, [1 1 10 10], ''true'');', invalidRecursive);
asserts.checkerror('setpixelposition(c, [1 1 10 10], [true false]);', invalidRecursive);
asserts.checkerror('setpixelposition(l, [1 1 10 10]);', noPosition);
asserts.checkerror('setpixelposition(groot, [1 1 10 10]);', noPosition);
d = uicontrol(f);
delete(d);
asserts.checkerror('setpixelposition(d, [1 1 10 10]);', invalidHandle);
close(f);
%=============================================================================
f = uifigure('Visible', 'off', 'Position', [100 100 500 400]);
p = uipanel(f, 'Position', [50 60 300 200]);
b = uibutton(p, 'Position', [10 12 60 20]);
setpixelposition(b, [5 6 60 20]);
asserts.isequal(b.Position, [5 6 60 20]);
setpixelposition(b, [70 90 60 20], true);
asserts.isequal(b.Position, [20 30 60 20]);
close(f);
%=============================================================================
