%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = warp(varargin)
  % warp(X, Y, Z, A)       display image A texture-mapped on surface X, Y, Z
  % warp(Z, A)             surface defined by Z, A auto X/Y from its size
  % warp(Z, A, map)        indexed image A with colormap map
  % warp(X, Y, Z, A, map)  indexed image A with colormap map on surface X, Y, Z
  % h = warp(...)          returns the surface handle
  narginchk(2, 5);
  nargoutchk(0, 1);

  inputArguments = convertStringToCharArgs(varargin);
  [X, Y, Z, A, map] = parseWarpInputs(inputArguments);

  ax = newplot();
  if ~isempty(map)
    colormap(ax, map);
  end
  h = surface(ax, X, Y, Z, A, 'FaceColor', 'texturemap', 'EdgeColor', 'none', ...
    'CDataMapping', 'direct');
  % warp displays the image the way an image is shown: rows increasing downward
  % (reversed Y axis) and the default 3-D view, so the mapped picture reads the
  % right way up on the surface.
  set(ax, 'YDir', 'reverse');
  view(ax, 3);

  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function [X, Y, Z, A, map] = parseWarpInputs(args)
  map = [];
  nb = numel(args);
  if (nb == 2)
    Z = args{1};
    A = args{2};
    [X, Y] = autoWarpGrid(Z);
  elseif (nb == 3)
    Z = args{1};
    A = args{2};
    map = args{3};
    [X, Y] = autoWarpGrid(Z);
  elseif (nb == 4)
    X = args{1};
    Y = args{2};
    Z = args{3};
    A = args{4};
  else
    X = args{1};
    Y = args{2};
    Z = args{3};
    A = args{4};
    map = args{5};
  end
  validateWarpSurface(X, Y, Z);
  validateWarpImage(A);
  if ~isempty(map)
    validateWarpColormap(map);
  end
end
%=============================================================================
function [X, Y] = autoWarpGrid(Z)
  if ~ismatrix(Z) || ndims(Z) > 2
    error('Nelson:warp:invalidZArgument', 'Z argument must be a matrix.');
  end
  [X, Y] = meshgrid(1:size(Z, 2), 1:size(Z, 1));
end
%=============================================================================
function validateWarpSurface(X, Y, Z)
  if ~isnumeric(Z) || ndims(Z) > 2
    error('Nelson:warp:invalidZArgument', 'Z argument must be a matrix.');
  end
  [Z_nr, Z_nc] = size(Z);
  if isvector(X) && isvector(Y)
    if (numel(Y) ~= Z_nr) || (numel(X) ~= Z_nc)
      error('Nelson:warp:invalidSizeZ', 'size(Z, 1) must be the same as length(Y) and size(Z, 2) must be the same as length(X).');
    end
  elseif ismatrix(X) && ismatrix(Y)
    if ~isequal(size(X), size(Z)) || ~isequal(size(Y), size(Z))
      error('Nelson:warp:invalidXYAnd', 'X, Y, and Z must have the same dimensions.');
    end
  else
    error('Nelson:warp:invalidXAndY', 'X and Y must be vectors and Z must be a matrix.');
  end
end
%=============================================================================
function validateWarpImage(A)
  if isempty(A) || ~(isnumeric(A) || islogical(A))
    error('Nelson:warp:invalidImage', 'A must be a non-empty indexed or truecolor image.');
  end
  np = size(A, 3);
  if ~(np == 1 || np == 3) || ndims(A) > 3
    error('Nelson:warp:invalidImage', 'A must be an indexed (M-by-N) or truecolor (M-by-N-by-3) image.');
  end
end
%=============================================================================
function validateWarpColormap(map)
  if ~isnumeric(map) || ndims(map) > 2 || size(map, 2) ~= 3
    error('Nelson:warp:invalidColormap', 'map must be an M-by-3 colormap matrix.');
  end
end
%=============================================================================
