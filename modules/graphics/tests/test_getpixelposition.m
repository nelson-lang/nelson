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
% a figure reports its Position in pixels, by handle or by number
asserts.isequal(getpixelposition(f), [100 100 500 400]);
asserts.isequal(getpixelposition(f, true), [100 100 500 400]);
asserts.isequal(getpixelposition(f.Number), [100 100 500 400]);
R = getpixelposition(f);
asserts.isequal(class(R), 'double');
asserts.isequal(size(R), [1 4]);
%=============================================================================
% the first pixel of the parent is 1
c = uicontrol(f, 'Style', 'pushbutton', 'Units', 'pixels', 'Position', [20 30 100 50]);
asserts.isequal(getpixelposition(c), [20 30 100 50]);
c.Units = 'normalized';
c.Position = [0.1 0.2 0.5 0.25];
asserts.isapprox(getpixelposition(c), [51 81 250 100], 1e-9);
% the object is left unchanged
asserts.isequal(c.Units, 'normalized');
asserts.isapprox(c.Position, [0.1 0.2 0.5 0.25], 1e-12);
delete(c);
p = uipanel(f, 'Units', 'normalized', 'Position', [0.1 0.2 0.5 0.25]);
asserts.isapprox(getpixelposition(p), [51 81 250 100], 1e-9);
delete(p);
%=============================================================================
% the character cell is 7 x 15 points
c = uicontrol(f, 'Style', 'pushbutton', 'Units', 'characters', 'Position', [10 10 10 10]);
asserts.isapprox(getpixelposition(c), [10 * 28 / 3 + 1, 201, 10 * 28 / 3, 200], 1e-9);
delete(c);
f.Units = 'characters';
asserts.isapprox(f.Position, [99 / (28 / 3), 99 / 20, 500 / (28 / 3), 20], 1e-9);
f.Units = 'pixels';
% every unit gives what Position reports once Units is 'pixels'
units = {'inches', 'centimeters', 'points', 'characters'};
values = {[1 0.5 2 1], [1 2 3 1.5], [36 72 144 40], [5 2 20 3]};
for k = 1:numel(units)
  c = uicontrol(f, 'Style', 'pushbutton', 'Units', units{k}, 'Position', values{k});
  R = getpixelposition(c);
  c.Units = 'pixels';
  asserts.isapprox(R, c.Position, 1e-9);
  delete(c);
end
%=============================================================================
ax = axes(f, 'Units', 'normalized', 'Position', [0.1 0.2 0.5 0.25]);
asserts.isapprox(getpixelposition(ax), [51 81 250 100], 1e-9);
asserts.isapprox(getpixelposition(ax, true), [51 81 250 100], 1e-9);
ax.Units = 'pixels';
ax.Position = [20 30 100 50];
asserts.isapprox(getpixelposition(ax), [20 30 100 50], 1e-9);
%=============================================================================
% objects without a four element Position in Units report zeros
l = line(ax, [0 1], [0 1]);
t = text(ax, 0.5, 0.5, 'x');
asserts.isequal(getpixelposition(l), [0 0 0 0]);
asserts.isequal(getpixelposition(t), [0 0 0 0]);
asserts.isequal(getpixelposition(groot), [0 0 0 0]);
asserts.isequal(getpixelposition([]), [0 0 0 0]);
delete(ax);
%=============================================================================
% recursive adds the origin of every container up to the figure (first pixel 1) and its border
outer = uipanel(f, 'Units', 'pixels', 'Position', [50 60 300 200], 'BorderType', 'none');
inner = uipanel(outer, 'Units', 'normalized', 'Position', [0.1 0.1 0.5 0.5]);
c = uicontrol(inner, 'Units', 'pixels', 'Position', [10 12 60 20]);
asserts.isequal(getpixelposition(outer, true), [50 60 300 200]);
innerPosition = getpixelposition(inner);
asserts.isapprox(innerPosition, [31 21 150 100], 1e-9);
asserts.isequal(getpixelposition(inner, false), innerPosition);
asserts.isequal(getpixelposition(inner, 0), innerPosition);
asserts.isapprox(getpixelposition(inner, true), [80 80 150 100], 1e-9);
asserts.isapprox(getpixelposition(inner, 1), [80 80 150 100], 1e-9);
asserts.isequal(getpixelposition(c), [10 12 60 20]);
innerRecursive = getpixelposition(inner, true);
innerBorder = getpixelposition(c, true) - [10 12 60 20] - [innerRecursive(1:2) - 1, 0, 0];
asserts.isapprox(innerBorder(3:4), [0 0], 1e-9);
asserts.istrue(all(innerBorder(1:2) >= 0) && all(innerBorder(1:2) <= 3));
%=============================================================================
invalidHandle = getString(message('Nelson:graphics:getpixelposition:invalidHandle'));
invalidRecursive = getString(message('Nelson:graphics:getpixelposition:invalidRecursive'));
asserts.checkerror('getpixelposition();', getString(message('Nelson:notEnoughInputs')));
asserts.checkerror('getpixelposition(c, true, 1);', getString(message('Nelson:tooManyInputs')));
asserts.checkerror('getpixelposition(42.5);', invalidHandle);
asserts.checkerror('getpixelposition(''a'');', invalidHandle);
asserts.checkerror('getpixelposition([outer, inner]);', invalidHandle);
asserts.checkerror('getpixelposition(c, ''true'');', invalidRecursive);
asserts.checkerror('getpixelposition(c, [true false]);', invalidRecursive);
asserts.checkerror('getpixelposition(c, []);', invalidRecursive);
d = uicontrol(f);
delete(d);
asserts.checkerror('getpixelposition(d);', invalidHandle);
%=============================================================================
% a figure in other units still reports pixels
f.Units = 'normalized';
asserts.isapprox(getpixelposition(f), [100 100 500 400], 1e-6);
close(f);
%=============================================================================
f = uifigure('Visible', 'off', 'Position', [100 100 500 400]);
p = uipanel(f, 'Position', [50 60 300 200]);
b = uibutton(p, 'Position', [10 12 60 20]);
asserts.isequal(getpixelposition(f), [100 100 500 400]);
asserts.isequal(getpixelposition(p), [50 60 300 200]);
asserts.isequal(getpixelposition(b), [10 12 60 20]);
asserts.isequal(getpixelposition(b, true), [60 72 60 20]);
close(f);
%=============================================================================
% a tiled layout places its axes in the frame of its own parent: no shift
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
t = tiledlayout(f, 2, 2);
ax = nexttile(t, 4);
drawnow();
asserts.isapprox(getpixelposition(ax, true), getpixelposition(ax), 1e-9);
layoutPosition = getpixelposition(t);
tilePosition = getpixelposition(ax, true);
asserts.istrue(tilePosition(1) + tilePosition(3) <= layoutPosition(1) + layoutPosition(3) + 1);
asserts.istrue(tilePosition(2) + tilePosition(4) <= layoutPosition(2) + layoutPosition(4) + 1);
close(f);
%=============================================================================
