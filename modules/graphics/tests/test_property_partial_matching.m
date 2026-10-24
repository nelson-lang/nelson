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
% A property name may be given as its shortest unambiguous leading substring,
% and an enumerated value likewise, matching the reference numerical language.
%=============================================================================
f = figure('Visible', 'off');
%=============================================================================
% property-name abbreviation on set()
ax = axes('Parent', f);
set(ax, 'pos', [0.1 0.1 0.8 0.8]);
asserts.isapprox(ax.Position, [0.1 0.1 0.8 0.8], 1e-12);
set(ax, 'colorm', gray(8));
asserts.isequal(size(ax.Colormap), [8 3]);
%=============================================================================
% property-name abbreviation during construction (primitives and containers)
l = line('Parent', ax, 'XData', [0 1], 'YData', [0 1], 'Col', [1 0 0], 'LineW', 2);
asserts.isapprox(l.Color, [1 0 0], 1e-12);
asserts.isapprox(l.LineWidth, 2, 1e-12);
t = text('Parent', ax, 'Pos', [0.5 0.5 0], 'Str', 'hi', 'FontW', 'bold');
asserts.isequal(t.FontWeight, 'bold');
ax2 = axes('pos', [0 0 1 1], 'Proj', 'perspective');
asserts.isequal(ax2.Projection, 'perspective');
f2 = figure('Vis', 'off', 'Color', 'w');
asserts.isequal(char(f2.Visible), 'off');
%=============================================================================
% enumerated-value abbreviation (set path, reaches the C++ property)
set(l, 'LineS', '--');
asserts.isequal(l.LineStyle, '--');
set(ax, 'Projection', 'o');
asserts.isequal(ax.Projection, 'orthographic');
p = patch('Parent', ax, 'XData', [0 1 1], 'YData', [0 0 1]);
set(p, 'EdgeColor', 'n');
asserts.isequal(p.EdgeColor, 'none');
set(p, 'FaceLighting', 'g');
asserts.isequal(p.FaceLighting, 'gouraud');
% the Units property (its own restricted-string subclass) abbreviates too,
% on both set() and dot-assignment
set(ax, 'Units', 'pi');
asserts.isequal(char(ax.Units), 'pixels');
ax.Units = 'n';
asserts.isequal(char(ax.Units), 'normalized');
%=============================================================================
% an ambiguous abbreviation is still rejected
asserts.checkerror('set(axes(), ''Po'', [0 0 1 1]);', graphicsPropertyErrorMessage('Po', 'Nelson:graphics:unrecognizedPropertySClass', 'Po', 'axes'));
%=============================================================================
% property-name abbreviation in the .m plotting wrappers: an abbreviated name
% must reach the same property the full name does, not be dropped by the
% wrapper's own name-based bookkeeping.
pa = patch([0 1 0], [0 0 1], 'r', 'EdgeC', [0 0 1], 'LineW', 3);
asserts.isapprox(pa.EdgeColor, [0 0 1], 1e-12);
asserts.isapprox(pa.LineWidth, 3, 1e-12);
su = surf(peaks(8), 'FaceA', 0.4, 'EdgeC', 'none');
asserts.isapprox(su.FaceAlpha, 0.4, 1e-12);
asserts.isequal(su.EdgeColor, 'none');
fi = fill([0 1 0], [0 0 1], 'g', 'LineW', 2, 'EdgeC', [0 0 1]);
asserts.isapprox(fi.LineWidth, 2, 1e-12);
asserts.isapprox(fi.EdgeColor, [0 0 1], 1e-12);
%=============================================================================
% the shared resolver exposed to the wrappers
asserts.isequal(canonicalGraphicsProperty('surface', 'FaceA'), 'FaceAlpha');
asserts.isequal(canonicalGraphicsProperty('patch', 'EdgeC'), 'EdgeColor');
asserts.isequal(canonicalGraphicsProperty('surface', 'Face'), '');
asserts.isequal(canonicalGraphicsProperty('axes', 'Position'), 'Position');
%=============================================================================
% property-name abbreviation on get(), matching what set() accepts
asserts.isequal(get(f, 'Pos'), get(f, 'Position'));
set(ax, 'XLim', [0 5]);
asserts.isapprox(get(ax, 'XLi'), [0 5], 1e-12);
set(ax, 'Projection', 'perspective');
asserts.isequal(get(ax, 'Proj'), 'perspective');
% an unknown property is still rejected on get()
asserts.checkerror('get(figure(''Visible'', ''off''), ''Nonexistent'');', getString(message('Nelson:graphics:unrecognizedPropertySClass', 'Nonexistent', 'figure')));
%=============================================================================
close all
%=============================================================================
