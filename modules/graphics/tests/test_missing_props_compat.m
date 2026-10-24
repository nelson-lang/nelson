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
% These graphics objects were missing properties that the reference numerical
% computing language exposes on the same object. Each property must exist with
% the reference storage class (and, where fixed, the reference default value).
%=============================================================================
close all
%=============================================================================
% fimplicit: line-series marker/color family.
h = fimplicit(@(x, y) x.^2 + y.^2 - 1);
assert_isequal(class(get(h, 'ColorMode')), 'char');
assert_isequal(get(h, 'ColorMode'), 'auto');
assert_isequal(class(get(h, 'LineStyleMode')), 'char');
assert_isequal(get(h, 'LineStyleMode'), 'auto');
assert_isequal(class(get(h, 'MarkerMode')), 'char');
assert_isequal(get(h, 'MarkerMode'), 'auto');
assert_isequal(class(get(h, 'MarkerEdgeColor')), 'char');
assert_isequal(get(h, 'MarkerEdgeColor'), 'auto');
assert_isequal(class(get(h, 'MarkerFaceColor')), 'char');
assert_isequal(get(h, 'MarkerFaceColor'), 'none');
assert_isequal(class(get(h, 'MarkerSize')), 'double');
assert_isequal(get(h, 'MarkerSize'), 6);
assert_isequal(class(get(h, 'SeriesIndex')), 'double');
assert_isequal(get(h, 'SeriesIndex'), 1);
close all
%=============================================================================
% fplot: ShowPoles.
h = fplot(@sin);
assert_isequal(class(get(h, 'ShowPoles')), 'nelson.lang.OnOffSwitchState');
assert_isequal(char(get(h, 'ShowPoles')), 'on');
close all
%=============================================================================
% heatmap: ColorScaling, Interpreter, Layout, MissingDataColor,
% PositionConstraint, XLimits, YLimits.
h = heatmap(magic(4));
assert_isequal(class(get(h, 'ColorScaling')), 'char');
assert_isequal(get(h, 'ColorScaling'), 'scaled');
assert_isequal(class(get(h, 'Interpreter')), 'char');
assert_isequal(get(h, 'Interpreter'), 'tex');
assert_isequal(class(get(h, 'MissingDataColor')), 'double');
assert_isequal(get(h, 'MissingDataColor'), [0.129411764705882, 0.129411764705882, 0.129411764705882]);
assert_isequal(class(get(h, 'PositionConstraint')), 'char');
assert_isequal(get(h, 'PositionConstraint'), 'outerposition');
assert_isequal(class(get(h, 'Layout')), 'nelson.ui.layout.LayoutOptions');
assert_isequal(class(get(h, 'XLimits')), 'cell');
assert_isequal(get(h, 'XLimits'), {'1', '4'});
assert_isequal(class(get(h, 'YLimits')), 'cell');
assert_isequal(get(h, 'YLimits'), {'1', '4'});
close all
%=============================================================================
% polaraxes: RAxis, ThetaAxis (NumericRuler decorators).
h = polaraxes();
assert_isequal(class(get(h, 'RAxis')), 'nelson.graphics.axis.decorator.NumericRuler');
assert_isequal(class(get(h, 'ThetaAxis')), 'nelson.graphics.axis.decorator.NumericRuler');
close all
%=============================================================================
% uidropdown, uitable, uitree: StyleConfigurations (empty 0x3 table).
f = uifigure();
d = uidropdown(f);
assert_isequal(class(get(d, 'StyleConfigurations')), 'table');
assert_isequal(size(get(d, 'StyleConfigurations')), [0 3]);
t = uitable(f);
assert_isequal(class(get(t, 'StyleConfigurations')), 'table');
assert_isequal(size(get(t, 'StyleConfigurations')), [0 3]);
assert_isequal(class(get(t, 'ColumnRearrangeable')), 'nelson.lang.OnOffSwitchState');
assert_isequal(char(get(t, 'ColumnRearrangeable')), 'off');
assert_isequal(class(get(t, 'DisplaySelection')), 'double');
assert_isequal(get(t, 'DisplaySelection'), []);
assert_istrue(isprop(t, 'Children'));
tr = uitree(f);
assert_isequal(class(get(tr, 'StyleConfigurations')), 'table');
assert_isequal(size(get(tr, 'StyleConfigurations')), [0 3]);
close all
%=============================================================================
