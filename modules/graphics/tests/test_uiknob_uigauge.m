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
fig = uifigure('Visible', 'off');
kb = uiknob(fig);
assert_isequal(class(kb), 'nelson.ui.control.Knob');
assert_isequal(kb.Type, 'uiknob');
assert_isequal(kb.Value, 0);
assert_isequal(kb.Limits, [0 100]);
assert_isequal(kb.Position, [100 100 60 60]);
assert_isequal(kb.MajorTicks, [0 20 40 60 80 100]);
assert_isequal(kb.MinorTicks, []);
kb.Value = 30;
assert_isequal(kb.Value, 30);
kb.Limits = [-100 100];
kb.Value = -30;
assert_isequal(kb.Value, -30);
%=============================================================================
dk = uiknob(fig, 'discrete');
assert_isequal(class(dk), 'nelson.ui.control.DiscreteKnob');
assert_isequal(dk.Type, 'uidiscreteknob');
assert_isequal(dk.Items, {'Off', 'Low', 'Medium', 'High'});
assert_isequal(dk.Value, 'Off');
assert_isequal(dk.ValueIndex, 1);
dk.Value = 'Medium';
assert_isequal(dk.Value, 'Medium');
assert_isequal(dk.ValueIndex, 3);
dk.ItemsData = [-10 0 10 20];
dk.Value = -10;
assert_isequal(dk.Value, -10);
%=============================================================================
g = uigauge(fig);
assert_isequal(class(g), 'nelson.ui.control.Gauge');
assert_isequal(g.Type, 'uigauge');
assert_isequal(g.Value, 0);
assert_isequal(g.Limits, [0 100]);
assert_isequal(g.Position, [100 100 120 120]);
assert_isequal(g.ScaleDirection, 'clockwise');
assert_isequal(g.MajorTicks, [0 20 40 60 80 100]);
assert_isequal(g.MinorTicks, []);
g.Value = 75;
assert_isequal(g.Value, 75);
g.Limits = [-100 100];
g.Value = -75;
assert_isequal(g.Value, -75);
%=============================================================================
lg = uigauge(fig, 'linear');
assert_isequal(class(lg), 'nelson.ui.control.LinearGauge');
assert_isequal(lg.Type, 'uilineargauge');
assert_isequal(lg.Orientation, 'horizontal');
assert_isequal(lg.Position, [100 100 120 40]);
%=============================================================================
ng = uigauge(fig, 'ninetydegree');
assert_isequal(class(ng), 'nelson.ui.control.NinetyDegreeGauge');
assert_isequal(ng.Orientation, 'northwest');
assert_isequal(ng.MajorTicks, [0 50 100]);
%=============================================================================
sg = uigauge(fig, 'semicircular');
assert_isequal(class(sg), 'nelson.ui.control.SemicircularGauge');
assert_isequal(sg.Orientation, 'north');
delete(fig);
close all
%=============================================================================
