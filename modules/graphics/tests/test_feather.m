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
f = figure('Visible', 'off');
h = feather([1 + 2i, 3 + 4i]);
asserts.istrue(all(isgraphics(h, 'line')));
asserts.isequal(size(h), [3 1]);
asserts.isequal(h(1).Type, 'line');
asserts.isequal(isnan(h(1).XData), [false false true false false false]);
asserts.isequal(isnan(h(1).YData), [false false true false false false]);
asserts.isequal(isnan(h(2).XData), [false false true false false false]);
asserts.isequal(isnan(h(2).YData), [false false true false false false]);
asserts.isapprox(h(1).XData(~isnan(h(1).XData)), [1 2 1.64 2 1.96], 1e-12);
asserts.isapprox(h(1).YData(~isnan(h(1).YData)), [0 2 1.68 2 1.52], 1e-12);
asserts.isapprox(h(2).XData(~isnan(h(2).XData)), [2 5 4.08 5 4.72], 1e-12);
asserts.isapprox(h(2).YData(~isnan(h(2).YData)), [0 4 3.44 4 2.96], 1e-12);
asserts.isequal(h(3).XData, [1 2]);
asserts.isequal(h(3).YData, [0 0]);
asserts.isequal(h(1).Marker, 'none');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
h = feather(ax, [1 3], [2 4], '--b', 'LineWidth', 2);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(2).Parent, ax);
asserts.isequal(h(3).Parent, ax);
asserts.isequal(h(1).LineWidth, 2);
asserts.isequal(h(1).LineStyle, '--');
asserts.isequal(h(1).Color, [0 0 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = feather([1 + 2i, 3 + 4i], '-or');
asserts.isequal(size(h), [3 1]);
asserts.isequal(h(1).Marker, 'o');
asserts.isequal(h(2).Marker, 'o');
asserts.isequal(h(3).Marker, 'o');
asserts.isequal(h(1).Color, [1 0 0]);
close(f);
%=============================================================================
asserts.checkerror('feather({}, 1);', getString(message('Nelson:dimagree:valueValueNumberElements', 'U', 'V')));
asserts.checkerror('feather([1 2], 1);', getString(message('Nelson:dimagree:valueValueNumberElements', 'U', 'V')));
asserts.checkerror('feather([1 2], [3 4], [5 6]);', getString(message('Nelson:graphics:nameValueArgumentsExpected')));
%=============================================================================
