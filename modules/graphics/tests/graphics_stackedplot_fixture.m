%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [T, rowTimes, temperature, pressure, rain, station] = graphics_stackedplot_fixture()
  temperature = [19.4; 20.1; 21.7; 23.2; 22.6; 20.9];
  pressure = [1012; 1011; 1009; 1008; 1010; 1013];
  rain = [0; 0.2; 0; 1.1; 0.4; 0];
  station = {'north'; 'north'; 'north'; 'north'; 'north'; 'north'};
  rowTimes = datetime(2026, 1, 1) + hours(0:5)';
  T = table(rowTimes, temperature, pressure, rain, station, ...
    'VariableNames', {'Time', 'Temperature', 'Pressure', 'Rain', 'Station'});
end
%=============================================================================
