%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
ts = timeseries([1; 2; 3], [10; 11; 12], 'Name', 'speed');
L = plot(ts);
asserts.istrue(isgraphics(L, 'line'));
asserts.isequal(L.XData, [10 11 12]);
asserts.isequal(L.YData, [1 2 3]);
ax = gca();
asserts.isequal(ax.Title.String, 'Time Series Plot:speed');
asserts.isequal(ax.XLabel.String, 'Time (seconds)');
asserts.isequal(ax.YLabel.String, 'speed');
%=============================================================================
f = figure();
ts.DataInfo.Units = 'm/s';
plot(ts);
ax = gca();
asserts.isequal(ax.YLabel.String, 'speed (m/s)');
%=============================================================================
f = figure();
ts = setinterpmethod(ts, 'zoh');
L = plot(ts, 'r');
asserts.istrue(isgraphics(L(1), 'stair'));
%=============================================================================
asserts.checkerror('plot(timeseries())', getString(message('Nelson:time:timeseriesContainTimeData')));
%=============================================================================
