%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Rebuilds a heatmap chart in place after one of its properties is dot-set.
% The C++ set builtin calls this with the heatmap group; it pulls the live
% property values over the stored options and redraws the internal primitives.
function __heatmap_refresh__(h)
  if numel(h) ~= 1 || ~isgraphics(h)
    return
  end
  state = get(h, 'UserData');
  if ~isstruct(state) || ~isfield(state, 'Options') || ~isfield(state, 'ColorData')
    return
  end
  opts = state.Options;
  liveProps = {'ColorLimits', 'ColorbarVisible', 'GridVisible', 'CellLabelFormat', ...
    'CellLabelColor', 'MissingDataLabel', 'ColorMethod', 'FontColor', 'FontSize', ...
    'Colormap', 'Title', 'XLabel', 'YLabel', 'Visible'};
  for k = 1:numel(liveProps)
    name = liveProps{k};
    try
      opts.(name) = get(h, name);
    catch
    end
  end
  ax = ancestor(h, 'axes');
  if isempty(ax)
    return
  end
  heatmapDrawContents(ax, h, state.ColorData, state.XData, state.YData, opts);
end
%=============================================================================
