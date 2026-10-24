%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function boundary = ip_trace_boundary_pixels(perim, startPoint, conn, firstStep, maxPoints)
  offsets8 = [-1 0; -1 1; 0 1; 1 1; 1 0; 1 -1; 0 -1; -1 -1];
  names8 = {'N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'};
  offsets4 = [-1 0; 0 1; 1 0; 0 -1];
  names4 = {'N', 'E', 'S', 'W'};
  if conn == 4
    offsets = offsets4;
    names = names4;
  else
    offsets = offsets8;
    names = names8;
  end
  firstIndex = find(strcmp(firstStep, names), 1);
  if isempty(firstIndex)
    error('Nelson:image_processing:unsupportedTraceDirection', 'Unsupported "%s" option.', 'trace direction');
  end
  remaining = perim;
  current = startPoint;
  boundary = zeros(0, 2);
  while remaining(current(1), current(2)) && size(boundary, 1) < maxPoints
    boundary = [boundary; current];
    remaining(current(1), current(2)) = false;
    nextPoint = [];
    for k = 0:size(offsets, 1) - 1
      offset = offsets(mod(firstIndex - 1 + k, size(offsets, 1)) + 1, :);
      candidate = current + offset;
      if candidate(1) >= 1 && candidate(2) >= 1 && candidate(1) <= size(perim, 1) ...
          && candidate(2) <= size(perim, 2) && remaining(candidate(1), candidate(2))
        nextPoint = candidate;
        break
      end
    end
    if isempty(nextPoint)
      [remRows, remCols] = find(remaining);
      if isempty(remRows)
        break
      end
      distances = (remRows - current(1)) .^ 2 + (remCols - current(2)) .^ 2;
      [~, pos] = min(distances);
      nextPoint = [remRows(pos), remCols(pos)];
    end
    current = nextPoint;
  end
end
%=============================================================================
