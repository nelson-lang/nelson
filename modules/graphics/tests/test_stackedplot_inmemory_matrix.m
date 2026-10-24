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
[~, rowTimes, temperature, pressure, rain, station] = graphics_stackedplot_fixture();
%=============================================================================
TT = timetable(rowTimes, temperature, pressure, rain, station, ...
  'VariableNames', {'Temperature', 'Pressure', 'Rain', 'Station'});
s = stackedplot(TT, {'Temperature', 'Rain'});
asserts.istrue(isgraphics(s, 'StackedLineChart'));
asserts.isequal(s.DisplayVariables, {'Temperature', 'Rain'});
asserts.isequal(s.DisplayLabels, {'Temperature'; 'Rain'});
asserts.isequal(char(s.GridVisible), 'off');
asserts.isequal(s.XLabel, 'rowTimes');
asserts.isequal(s.Children(1).Children(end).XData, datenum(rowTimes)');
close all;
%=============================================================================
