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
f = figure('Visible', 'off');
groups = {'a', 'b', 'a', 'b', 'a', 'b'};
h = scatterhistogram(f, x, y, 'GroupData', groups, 'LegendVisible', 'on', ...
  'Color', {'red', 'blue'}, 'MarkerStyle', {'o', 's'});
state = h.UserData;
asserts.isequal(state.Groups.Count, 2);
asserts.isequal(length(state.Scatter), 2);
asserts.isequal(state.Scatter(1).DisplayName, 'a');
asserts.isequal(state.Scatter(2).DisplayName, 'b');
asserts.isequal(state.Scatter(2).Marker, 'square');
close(f);
close all
%=============================================================================
