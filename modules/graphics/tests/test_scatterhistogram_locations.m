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
[x, y] = graphics_scatterhistogram_fixture();
locations = {'SouthWest', 'SouthEast', 'NorthEast', 'NorthWest'};
for k = 1:numel(locations)
  f = figure('Visible', 'off');
  h = scatterhistogram(f, x, y, 'ScatterPlotLocation', locations{k});
  asserts.isequal(h.ScatterPlotLocation, locations{k});
  asserts.isequal(length(h.Children), 3);
  close(f);
end
close all
%=============================================================================
