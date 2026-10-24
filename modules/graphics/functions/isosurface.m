%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isosurface(varargin)
  narginchk(1, 8);
  nargoutchk(0, 3);
  args = convertStringToCharArgs(varargin);
  [x, y, z, V, isovalue, colors, noshare, verbose] = parseIsosurfaceInputs(args);
  if isempty(isovalue)
    isovalue = automaticIsosurfaceLevel(V);
  end
  if verbose
    disp(_('Computing isosurface...'));
  end
  if isempty(isovalue)
    faces = zeros(0, 3);
    vertices = zeros(0, 3);
    cdata = [];
    normals = zeros(0, 3);
  else
    [faces, vertices, cdata, normals] = __isosurface__(x, y, z, V, isovalue, colors, noshare);
  end
  if nargout == 0
    renderIsosurface(faces, vertices, cdata, normals, isovalue, x, y, z);
  elseif nargout == 1
    varargout{1} = isosurfaceStruct(faces, vertices, cdata);
  else
    varargout{1} = faces;
    varargout{2} = vertices;
    if nargout == 3
      varargout{3} = cdata;
    end
  end
end
%=============================================================================
function renderIsosurface(faces, vertices, cdata, normals, isovalue, x, y, z)
  ax = newplot();
  if isempty(cdata)
    colorLimit = isosurfaceColorLimit(isovalue);
    ax.CLim = [0, colorLimit];
    faceVertexCData = repmat(isosurfaceLevelColor(ax, isovalue, colorLimit), size(vertices, 1), 1);
  else
    faceVertexCData = cdata;
  end
  p = patch(ax, 'Faces', faces, 'Vertices', vertices, 'FaceVertexCData', faceVertexCData, ...
    'FaceColor', 'flat', 'FaceLighting', 'gouraud', 'EdgeColor', 'none', 'UserData', isovalue);
  if ~isempty(normals)
    p.VertexNormals = normals;
    p.VertexNormalsMode = 'manual';
  end
  applyIsosurfaceAxes(ax, x, y, z, isovalue);
end
%=============================================================================
function applyIsosurfaceAxes(ax, x, y, z, isovalue)
  ax.XLim = [min(x), max(x)];
  ax.YLim = [min(y), max(y)];
  ax.ZLim = [min(z), max(z)];
  ax.CLim = [0, isosurfaceColorLimit(isovalue)];
  view(ax, 3);
  camlight(ax);
end
%=============================================================================
function colorLimit = isosurfaceColorLimit(isovalue)
  colorLimit = 2 * max(abs(isovalue), eps());
end
%=============================================================================
function color = isosurfaceLevelColor(ax, isovalue, colorLimit)
  colors = colormap(ax);
  nbColors = size(colors, 1);
  index = ceil(isovalue ./ colorLimit .* nbColors);
  index = max(1, min(nbColors, index));
  color = colors(index, :);
end
%=============================================================================
function s = isosurfaceStruct(faces, vertices, cdata)
  s = struct('vertices', vertices, 'faces', faces);
  if ~isempty(cdata)
    s.facevertexcdata = cdata;
  end
end
%=============================================================================
