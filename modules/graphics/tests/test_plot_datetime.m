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
% plot with datetime on the X axis
f = figure('Visible', 'off');
t = datetime(2020, 1, 1) + days(0:9);
h = plot(t, 1:10);
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
% converted serial numbers must match datenum of the datetime range
assert_isequal(min(xt) >= datenum(2020, 1, 1) - 1, true);
close(f);
%=============================================================================
% plot with datetime on the Y axis
f = figure('Visible', 'off');
t = datetime(2021, 3, 1) + days(0:4);
h = plot(1:5, t);
assert_istrue(~isempty(h));
ax = gca();
yt = get(ax, 'YTick');
assert_istrue(~isempty(yt));
assert_istrue(all(diff(yt) > 0));
labels = get(ax, 'YTickLabel');
assert_istrue(~isempty(labels));
close(f);
%=============================================================================
% single argument plot(datetime) maps the series to the Y axis
f = figure('Visible', 'off');
t = datetime(2022, 6, 1) + days(0:6);
h = plot(t);
assert_istrue(~isempty(h));
ax = gca();
yt = get(ax, 'YTick');
assert_istrue(~isempty(yt));
close(f);
%=============================================================================
% line with datetime X
f = figure('Visible', 'off');
t = datetime(2020, 1, 1) + days(0:9);
h = line(t, 1:10);
assert_istrue(~isempty(h));
ax = gca();
xt = get(ax, 'XTick');
assert_istrue(~isempty(xt));
assert_istrue(all(diff(xt) > 0));
close(f);
%=============================================================================
% scatter with datetime X
f = figure('Visible', 'off');
t = datetime(2020, 1, 1) + days(0:9);
h = scatter(t, 1:10);
assert_istrue(~isempty(h));
ax = gca();
xt = get(ax, 'XTick');
assert_istrue(~isempty(xt));
assert_istrue(all(diff(xt) > 0));
close(f);
%=============================================================================
% stairs with datetime X
f = figure('Visible', 'off');
t = datetime(2020, 1, 1) + days(0:9);
h = stairs(t, 1:10);
assert_istrue(~isempty(h));
ax = gca();
xt = get(ax, 'XTick');
assert_istrue(~isempty(xt));
close(f);
%=============================================================================
% area with datetime X
f = figure('Visible', 'off');
t = datetime(2020, 1, 1) + days(0:9);
h = area(t, (1:10)');
assert_istrue(~isempty(h));
ax = gca();
xt = get(ax, 'XTick');
assert_istrue(~isempty(xt));
close(f);
%=============================================================================
close all
%=============================================================================
