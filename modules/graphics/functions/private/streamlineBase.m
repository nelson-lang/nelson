%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [h, state] = streamlineBase(varargin)
  args = convertStringToCharArgs(varargin);
  [parent, args] = graphicsParseParent(args);
  [dataArgs, tail] = splitStreamProperties(args);
  % The vertices are computed before any axes is created: a call that cannot be
  % parsed leaves no empty figure behind.
  [vertices, field] = streamFieldVertices(dataArgs);
  ax = graphicsAddTargetAxes(parent);
  if isempty(parent)
    parent = ax;
  end
  h = lineFromVertices(parent, vertices, tail);
  state = field;
  state.Handles = h;
end
%=============================================================================
function [dataArgs, tail] = splitStreamProperties(args)
  firstText = find(cellfun('isclass', args, 'char'), 1);
  if isempty(firstText)
    dataArgs = args;
    tail = {};
  else
    dataArgs = args(1:firstText - 1);
    tail = args(firstText:end);
  end
end
%=============================================================================
function h = lineFromVertices(parent, vertices, tail)
  h = gobjects(numel(vertices), 1);
  for k = 1:numel(vertices)
    if ~isempty(vertices{k})
      h(k) = lineFromPoints(parent, vertices{k}, tail);
    end
  end
  h(~isgraphics(h)) = [];
end
%=============================================================================
function h = lineFromPoints(parent, pts, tail)
  tail = streamLineProperties(tail);
  if size(pts, 2) == 3
    h = line('Parent', parent, 'XData', pts(:, 1), 'YData', pts(:, 2), ...
      'ZData', pts(:, 3), tail{:});
  else
    h = line('Parent', parent, 'XData', pts(:, 1), 'YData', pts(:, 2), tail{:});
  end
end
%=============================================================================
function tail = streamLineProperties(tail)
  if isempty(tail) || streamLineCanUseDefaultColor(tail)
    tail = [{'Color', [0.066, 0.443, 0.745]}, tail];
  end
end
%=============================================================================
function tf = streamLineCanUseDefaultColor(tail)
  tf = true;
  if rem(length(tail), 2) ~= 0
    tf = false;
    return
  end
  for k = 1:2:length(tail)
    if ischar(tail{k}) && strcmpi(tail{k}, 'color')
      tf = false;
      return
    end
  end
end
%=============================================================================
