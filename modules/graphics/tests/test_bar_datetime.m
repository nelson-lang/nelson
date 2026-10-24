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
% bar with datetime on the X axis
f = figure('Visible', 'off');
t = datetime(2020, 1, 1) + days(0:9);
h = bar(t, 1:10);
assert_istrue(~isempty(h));
ax = gca();
xt = get(ax, 'XTick');
assert_istrue(~isempty(xt));
assert_istrue(all(diff(xt) > 0));
xl = get(ax, 'XLim');
assert_istrue(all(xt >= xl(1) - eps) & all(xt <= xl(2) + eps));
labels = get(ax, 'XTickLabel');
assert_isequal(class(labels), 'cell');
assert_istrue(~isempty(labels));
% bar positions must be the serial date numbers of the datetime range
xdata = get(h(1), 'XData');
assert_isequal(xdata(:).', datenum(t));
close(f);
%=============================================================================
% barh with datetime on the position (Y) axis
f = figure('Visible', 'off');
t = datetime(2021, 3, 1) + days(0:4);
h = barh(t, 1:5);
assert_istrue(~isempty(h));
ax = gca();
yt = get(ax, 'YTick');
assert_istrue(~isempty(yt));
assert_istrue(all(diff(yt) > 0));
labels = get(ax, 'YTickLabel');
assert_istrue(~isempty(labels));
close(f);
%=============================================================================
% grouped bar with datetime X and a multi-series Y matrix
f = figure('Visible', 'off');
t = datetime(2022, 6, 1) + days(0:3);
h = bar(t, [1 2; 3 4; 5 6; 7 8]);
assert_isequal(numel(h), 2);
ax = gca();
xt = get(ax, 'XTick');
assert_istrue(~isempty(xt));
assert_istrue(all(diff(xt) > 0));
close(f);
%=============================================================================
% non datetime bar keeps numeric ticks (non-regression)
f = figure('Visible', 'off');
h = bar([2 4 6 8]);
assert_istrue(~isempty(h));
ax = gca();
xt = get(ax, 'XTick');
assert_istrue(~isempty(xt));
close(f);
%=============================================================================
close all
%=============================================================================
