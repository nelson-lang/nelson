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
% Object-valued chart properties must report a typed class, not a bare double
% or the generic GraphicsPlaceholder.
%=============================================================================
close all
%=============================================================================
% Annotation: event-data object exposed by every chart primitive.
f = figure('Visible', 'off');
h = area(1:5);
a = h.Annotation;
assert_isequal(class(a), 'nelson.graphics.eventdata.Annotation');
assert_isequal(size(a), [1 1]);
li = a.LegendInformation;
assert_isequal(class(li), 'nelson.graphics.eventdata.LegendEntry');
assert_isequal(li.IconDisplayStyle, 'on');
close(f);
%=============================================================================
% BaseLine: axis decorator object on area / bar / stem.
f = figure('Visible', 'off');
h = area(1:5);
bl = h.BaseLine;
assert_isequal(class(bl), 'nelson.graphics.axis.decorator.Baseline');
assert_isequal(size(bl), [1 1]);
assert_isequal(bl.LineStyle, '-');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = bar(1:5);
assert_isequal(class(h.BaseLine), 'nelson.graphics.axis.decorator.Baseline');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = stem(1:5);
assert_isequal(class(h.BaseLine), 'nelson.graphics.axis.decorator.Baseline');
close(f);
%=============================================================================
% DataTipTemplate: data-tip object on chart primitives.
f = figure('Visible', 'off');
h = plot(1:10);
dt = h.DataTipTemplate;
assert_isequal(class(dt), 'nelson.graphics.datatip.DataTipTemplate');
assert_isequal(size(dt), [1 1]);
assert_isequal(dt.Interpreter, 'tex');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = scatter(1:5, 1:5);
assert_isequal(class(h.DataTipTemplate), 'nelson.graphics.datatip.DataTipTemplate');
close(f);
%=============================================================================
% Layout: empty typed LayoutOptions placeholder on an axes until a tiled
% layout populates it.
f = figure('Visible', 'off');
ax = axes('Parent', f);
lay = ax.Layout;
assert_isequal(class(lay), 'nelson.ui.layout.LayoutOptions');
assert_istrue(isempty(lay));
close(f);
%=============================================================================
% SourceTable: empty table (not an empty double) until a chart is built from a
% table.
f = figure('Visible', 'off');
h = scatter(1:5, 1:5);
st = h.SourceTable;
assert_isequal(class(st), 'table');
assert_istrue(isempty(st));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = stem(1:5);
assert_isequal(class(h.SourceTable), 'table');
close(f);
%=============================================================================
% stackedplot reports its source as an empty timetable, not a table.
f = figure('Visible', 'off');
h = stackedplot([1 2 3; 4 5 6]);
st = h.SourceTable;
assert_isequal(class(st), 'timetable');
assert_istrue(isempty(st));
close(f);
%=============================================================================
