%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
% Names supplied by the caller: wedge labels read "<name> (<percent>)".
f = figure('Visible', 'off');
p = piechart([4 3 2], ["A", "B", "C"]);
asserts.isequal(p.Labels, ["A (44.4%)", "B (33.3%)", "C (22.2%)"]);
asserts.isequal(p.NamesMode, 'manual');
asserts.isequal(p.LabelStyleMode, 'auto');
close(f);
%=============================================================================
% Auto-generated names keep the bare percentage.
f = figure('Visible', 'off');
p = piechart([1 2 3]);
asserts.isequal(p.Labels, ["16.7%", "33.3%", "50%"]);
close(f);
%=============================================================================
% An explicit LabelStyle keeps the bare value even when names are supplied.
f = figure('Visible', 'off');
p = piechart([4 3 2], ["A", "B", "C"]);
p.LabelStyle = 'data';
drawnow();
asserts.isequal(p.LabelStyleMode, 'manual');
asserts.isequal(p.Labels, ["4", "3", "2"]);
close(f);
%=============================================================================
% Donut charts share the same named-label rule.
f = figure('Visible', 'off');
d = donutchart([4 3 2], ["A", "B", "C"]);
asserts.isequal(d.Labels, ["A (44.4%)", "B (33.3%)", "C (22.2%)"]);
close(f);
close all
%=============================================================================
