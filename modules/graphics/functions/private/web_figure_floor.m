%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function web_figure_floor(ax, enabled)
  existing = findall(ax, 'Tag', '__web_floor_projection__');
  if ~isempty(existing)
    delete(existing);
  end
  if ~enabled
    return;
  end
  limits = get(ax, 'ZLim');
  objects = findall(ax, 'Type', 'line');
  for k = 1:numel(objects)
    object = objects(k);
    x = get(object, 'XData');
    y = get(object, 'YData');
    line('Parent', ax, 'XData', x, 'YData', y, ...
      'ZData', limits(1) * ones(size(x)), 'Color', [0.6 0.6 0.6], ...
      'LineStyle', ':', 'Tag', '__web_floor_projection__', 'UserData', object, ...
      'HandleVisibility', 'off', 'HitTest', 'off', 'PickableParts', 'none');
  end
end
