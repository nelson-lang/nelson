%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surf2patch(varargin)
  narginchk(1, 5);
  nargoutchk(0, 3);

  % Optional trailing 'triangles'/'quads' selector.
  triangulate = false;
  args = varargin;
  if ~isempty(args) && (ischar(args{end}) || isStringScalar(args{end}))
    selector = lower(char(args{end}));
    if strcmp(selector, 'triangles')
      triangulate = true;
      args(end) = [];
    elseif strcmp(selector, 'quads')
      args(end) = [];
    else
      error('Nelson:surf2patch:invalidOption', ...
        'Last argument must be ''triangles'' or ''quads''.');
    end
  end

  [X, Y, Z, C] = parseSurfaceData(args);

  [numRows, numColumns] = size(Z);
  if numRows < 2 || numColumns < 2
    error('Nelson:surf2patch:tooSmall', ...
      'Z must have at least two rows and two columns.');
  end

  vertices = [X(:), Y(:), Z(:)];
  faces = buildFaces(numRows, numColumns, triangulate);
  faceVertexCData = buildCData(C, numRows, numColumns);

  if nargout <= 1
    fvc.faces = faces;
    fvc.vertices = vertices;
    fvc.facevertexcdata = faceVertexCData;
    varargout{1} = fvc;
  else
    varargout{1} = faces;
    varargout{2} = vertices;
    if nargout >= 3
      varargout{3} = faceVertexCData;
    end
  end
end
%=============================================================================
function [X, Y, Z, C] = parseSurfaceData(args)
  if numel(args) == 1 && isscalar(args{1}) && isgraphics(args{1})
    h = args{1};
    X = get(h, 'XData');
    Y = get(h, 'YData');
    Z = get(h, 'ZData');
    C = get(h, 'CData');
    [X, Y] = expandGrid(X, Y, Z);
    if isempty(C)
      C = Z;
    end
    return
  end
  switch numel(args)
    case 1
      Z = args{1};
      C = Z;
      [X, Y] = defaultGrid(Z);
    case 2
      Z = args{1};
      C = args{2};
      [X, Y] = defaultGrid(Z);
    case 3
      X = args{1};
      Y = args{2};
      Z = args{3};
      C = Z;
    case 4
      X = args{1};
      Y = args{2};
      Z = args{3};
      C = args{4};
    otherwise
      error('Nelson:surf2patch:invalidInput', 'Wrong number of input arguments.');
  end
  [X, Y] = expandGrid(X, Y, Z);
end
%=============================================================================
function [X, Y] = defaultGrid(Z)
  [numRows, numColumns] = size(Z);
  [X, Y] = meshgrid(1:numColumns, 1:numRows);
end
%=============================================================================
function [X, Y] = expandGrid(X, Y, Z)
  [numRows, numColumns] = size(Z);
  if isvector(X) && ~isequal(size(X), size(Z))
    X = repmat(X(:).', numRows, 1);
  end
  if isvector(Y) && ~isequal(size(Y), size(Z))
    Y = repmat(Y(:), 1, numColumns);
  end
end
%=============================================================================
function faces = buildFaces(numRows, numColumns, triangulate)
  [rowIndex, columnIndex] = meshgrid(1:(numRows - 1), 1:(numColumns - 1));
  rowIndex = rowIndex(:);
  columnIndex = columnIndex(:);
  % Column-major linear indices of the four cell corners.
  v1 = rowIndex + (columnIndex - 1) * numRows;
  v2 = (rowIndex + 1) + (columnIndex - 1) * numRows;
  v3 = (rowIndex + 1) + columnIndex * numRows;
  v4 = rowIndex + columnIndex * numRows;
  if triangulate
    faces = [v1, v2, v3; v1, v3, v4];
  else
    faces = [v1, v2, v3, v4];
  end
end
%=============================================================================
function faceVertexCData = buildCData(C, numRows, numColumns)
  if isempty(C)
    faceVertexCData = [];
    return
  end
  if ndims(C) == 3
    faceVertexCData = reshape(C, numRows * numColumns, size(C, 3));
  else
    faceVertexCData = C(:);
  end
end
%=============================================================================
