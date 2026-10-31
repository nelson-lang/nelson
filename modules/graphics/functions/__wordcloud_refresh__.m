%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Rebuilds a word cloud in place after one of its properties is dot-set. The
% C++ set builtin calls this with the chart; it pulls the live property values
% over the stored options, deletes the old words and lays them out again.
function __wordcloud_refresh__(h)
  if numel(h) ~= 1 || ~isgraphics(h)
    return
  end
  state = get(h, 'UserData');
  if ~isstruct(state) || ~isfield(state, 'Options') || ~isfield(state, 'Axes') ...
      || ~isfield(state, 'Words') || ~isfield(state, 'SizeData')
    return
  end
  ax = state.Axes;
  if ~isgraphics(ax)
    return
  end
  opts = state.Options;
  liveProps = {'MaxDisplayWords', 'Color', 'HighlightColor', 'Shape', 'LayoutNum', ...
    'SizePower', 'FontName', 'Box', 'Title', 'TitleFontName'};
  for k = 1:numel(liveProps)
    name = liveProps{k};
    try
      opts.(name) = get(h, name);
    catch
    end
  end
  if isfield(state, 'TextHandles')
    old = state.TextHandles;
    for k = 1:numel(old)
      if isgraphics(old(k))
        delete(old(k));
      end
    end
  end
  fig = ancestor(ax, 'figure');
  if isgraphics(fig)
    fig.SizeChangedFcn = '';
  end
  textHandles = wordcloudDrawCloud(ax, state.Words, state.SizeData, opts);
  state.Options = opts;
  state.TextHandles = textHandles;
  set(h, 'UserData', state);
end
%=============================================================================
