%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
skip_if_portable_graphics_backend('stackedplot text table import is not stable yet.');
testsPath = [modulepath('graphics'), '/tests'];
%=============================================================================
weather = readtimetable([testsPath, '/fixtures/stackedplot_weather_events.csv']);
eventTable = eventtable(timetable(weather.Time, weather.EventType, ...
  hours(weather.EventLengthHours), weather.Precipitation, ...
  'VariableNames', {'EventType', 'EventLength', 'Precipitation'}), ...
  'EventLabelsVariable', 'EventType', ...
  'EventLengthsVariable', 'EventLength');
weatherData = timetable(datetime(2022, 11, (1:15)'), ...
  [36; 31; 37; 36; 38; 32; 35; 34; 32; 30; 39; 34; 31; 40; 34], ...
  [45; 76; 43; 46; 72; 54; 50; 45; 72; 58; 54; 58; 73; 78; 66], ...
  'VariableNames', {'Temperature', 'Humidity'});
weatherData.Properties.Events = eventTable;
s = stackedplot(weatherData);
asserts.isequal(char(s.EventsVisible), 'on');
asserts.isequal(s.DisplayVariables, {'Temperature', 'Humidity'});
eventPatches = stackedplotEventObjects(s);
asserts.isequal(length(eventPatches), 8);
close all;
s = stackedplot(weatherData, 'EventsVisible', 'off');
asserts.isequal(char(s.EventsVisible), 'off');
eventPatches = stackedplotEventObjects(s);
asserts.isequal(length(eventPatches), 0);
close all;
%=============================================================================
function eventPatches = stackedplotEventObjects(s)
  eventPatches = [];
  axesHandles = s.Children;
  for k = 1:numel(axesHandles)
    eventPatches = [eventPatches; findobj(axesHandles(k), 'Tag', 'StackedplotEvent')];
  end
end
%=============================================================================
