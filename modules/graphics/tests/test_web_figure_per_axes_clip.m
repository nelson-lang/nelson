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
% Each axes clips its own children to its own rectangle in the web display list
% (regression: a Visible 'off' axes' line was clipped to the last axes, not its).
%=============================================================================
f = figure('Visible', 'off');
tops = [0.70, 0.40, 0.10];
for k = 1:numel(tops)
  ax = axes('Parent', f, 'Position', [0.10, tops(k), 0.80, 0.20], 'Visible', 'off');
  set(ax, 'XLim', [0 1], 'YLim', [0 1]);
  line('Parent', ax, 'XData', [0 1], 'YData', [0.5 0.5], 'Color', [0 0 0]);
end
document = jsondecode(__web_display_list__(f));
primitives = document.primitives;
%=============================================================================
% Walk the primitive stream, tracking the active clip rectangle, and record the
% clip in force as each polyline (a line) is emitted.
currentClip = [];
savedClips = {};
lineClipTops = [];
for k = 1:numel(primitives)
  if iscell(primitives)
    p = primitives{k};
  else
    p = primitives(k);
  end
  switch p.kind
    case 'clip'
      currentClip = [p.x, p.y, p.w, p.h];
    case 'clipSave'
      savedClips{end + 1} = currentClip;
    case 'clipRestore'
      if isempty(savedClips)
        currentClip = [];
      else
        currentClip = savedClips{end};
        savedClips(end) = [];
      end
    case {'polyline', 'line', 'segments'}
      assert_istrue(~isempty(currentClip));
      lineClipTops = [lineClipTops; currentClip(2)];
  end
end
%=============================================================================
% Three lines, each under its own axes' clip: three distinct clip rectangles,
% not all three collapsed onto the last axes.
assert_isequal(numel(lineClipTops), 3);
assert_isequal(numel(unique(round(lineClipTops))), 3);
%=============================================================================
close(f);
%=============================================================================
