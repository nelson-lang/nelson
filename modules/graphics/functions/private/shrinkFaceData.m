%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [newFaces, newVertices] = shrinkFaceData(faces, vertices, sf)
  scale = sqrt(sf);
  newFaces = NaN(size(faces));
  newVertices = zeros(0, size(vertices, 2));
  nextIndex = 1;
  for row = 1:size(faces, 1)
    [newFaces, newVertices, nextIndex] = shrinkFaceRow(...
      faces, vertices, row, scale, newFaces, newVertices, nextIndex);
  end
end
%=============================================================================
function [newFaces, newVertices, nextIndex] = shrinkFaceRow(...
    faces, vertices, row, scale, newFaces, newVertices, nextIndex)
  ids = faces(row, isfinite(faces(row, :)));
  if isempty(ids)
    return
  end
  shrunk = shrinkFaceVertices(vertices(ids, :), scale);
  count = size(shrunk, 1);
  newVertices(nextIndex:nextIndex + count - 1, :) = shrunk;
  newFaces(row, 1:count) = nextIndex:nextIndex + count - 1;
  nextIndex = nextIndex + count;
end
%=============================================================================
function shrunk = shrinkFaceVertices(points, scale)
  center = mean(points, 1);
  shrunk = center + scale * (points - center);
end
%=============================================================================
