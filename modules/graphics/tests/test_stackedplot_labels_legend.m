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
indoors = readtimetable([testsPath, '/fixtures/stackedplot_inside.csv']);
outdoors = readtimetable([testsPath, '/fixtures/stackedplot_weather.csv']);
s = stackedplot(outdoors, ...
  'Title', 'Weather Data', ...
  'DisplayLabels', {'RH (%)', 'T (F)', 'P (in Hg)'});
asserts.isequal(s.Title, 'Weather Data');
asserts.isequal(s.DisplayLabels, {'RH (%)'; 'T (F)'; 'P (in Hg)'});
asserts.isequal(s.Children(1).Title.String, 'Weather Data');
asserts.isequal(s.Children(2).Children(end - 1).String, 'T (F)');
close all;
%=============================================================================
s = stackedplot(indoors, outdoors, ...
  'LegendLabels', {'Inside Readings', 'Outside Readings'}, ...
  'LegendOrientation', 'vertical', ...
  'LegendVisible', 'on');
asserts.isequal(s.LegendLabels, {'Inside Readings', 'Outside Readings'});
asserts.isequal(s.LegendOrientation, 'vertical');
legendAxes = findobj(gcf(), 'Tag', 'legend');
asserts.istrue(~isempty(legendAxes));
close all;
%=============================================================================
