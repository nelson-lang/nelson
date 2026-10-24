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
%=============================================================================
f = figure('Visible', 'off');
plot([datenum(2000, 1, 1) datenum(2020, 1, 1)], [0 1]);
ax = gca();
set(ax, 'XTick', [datenum(2000, 1, 1), datenum(2010, 1, 1), datenum(2020, 1, 1)]);
datetick('x', 'yyyy', 'keepticks', 'keeplimits');
assert_isequal(ax.XTickLabel', {'2000', '2010', '2020'});
close(f);
%=============================================================================
f = figure('Visible', 'off');
plot([datenum(2021, 1, 1) datenum(2021, 1, 3)], [0 1]);
ax = gca();
set(ax, 'YTick', [datenum(2021, 1, 1), datenum(2021, 1, 2), datenum(2021, 1, 3)]);
plot(ax, [0 1], [datenum(2021, 1, 1) datenum(2021, 1, 3)]);
set(ax, 'YTick', [datenum(2021, 1, 1), datenum(2021, 1, 2), datenum(2021, 1, 3)]);
datetick('y', 'mm/dd', 'keepticks', 'keeplimits');
assert_isequal(ax.YTickLabel', {'01/01', '01/02', '01/03'});
close(f);
%=============================================================================
close all
%=============================================================================
