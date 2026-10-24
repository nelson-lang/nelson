%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function se = strel(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  if ~(ischar(varargin{1}) || isstring(varargin{1}))
    nhood = ip_nhood(varargin{1});
    se = struct('type', 'arbitrary', 'nhood', nhood);
    return;
  end
  shape = lower(convertStringsToChars(varargin{1}));
  switch shape
    case 'arbitrary'
      if nargin ~= 2
        error('Nelson:image_processing:arbitraryStructuringElementRequiresNeighborhood', '%s structuring element requires %s.', 'Arbitrary', 'a neighborhood');
      end
      nhood = ip_nhood(varargin{2});
    case 'square'
      if nargin ~= 2
        error('Nelson:image_processing:squareStructuringElementRequiresSize', '%s structuring element requires %s.', 'Square', 'a size');
      end
      n = varargin{2};
      if ~isscalar(n) || ~isfinite(double(n)) || n < 1 || n ~= floor(n)
        error('Nelson:image_processing:squareSizePositiveIntegerScalar', '%s must be a positive integer scalar.', 'Square size');
      end
      nhood = true(n, n);
    case 'rectangle'
      if nargin ~= 2
        error('Nelson:image_processing:rectangleStructuringElementRequiresSizeVector', '%s structuring element requires %s.', 'Rectangle', 'a size vector');
      end
      sz = varargin{2};
      if numel(sz) ~= 2 || any(~isfinite(double(sz(:)))) || any(sz(:) < 1) ...
          || any(sz(:) ~= floor(sz(:)))
        error('Nelson:image_processing:rectangleSizeContainPositiveIntegers', '%s size must contain positive integers.', 'Rectangle');
      end
      nhood = true(sz(1), sz(2));
    case 'cube'
      if nargin ~= 2
        error('Nelson:image_processing:cubeStructuringElementRequiresSize', '%s structuring element requires %s.', 'Cube', 'a size');
      end
      n = varargin{2};
      if ~isscalar(n) || ~isfinite(double(n)) || n < 1 || n ~= floor(n)
        error('Nelson:image_processing:cubeSizePositiveIntegerScalar', '%s must be a positive integer scalar.', 'Cube size');
      end
      nhood = true(n, n, n);
    case 'cuboid'
      if nargin ~= 2
        error('Nelson:image_processing:cuboidStructuringElementRequiresSizeVector', '%s structuring element requires %s.', 'Cuboid', 'a size vector');
      end
      sz = varargin{2};
      if numel(sz) ~= 3 || any(~isfinite(double(sz(:)))) || any(sz(:) < 1) ...
          || any(sz(:) ~= floor(sz(:)))
        error('Nelson:image_processing:cuboidSizeContainPositiveIntegers', '%s size must contain positive integers.', 'Cuboid');
      end
      nhood = true(sz(1), sz(2), sz(3));
    case 'sphere'
      if nargin ~= 2
        error('Nelson:image_processing:sphereStructuringElementRequiresRadius', '%s structuring element requires %s.', 'Sphere', 'a radius');
      end
      r = varargin{2};
      if ~isscalar(r) || ~isfinite(double(r)) || r < 0 || r ~= floor(r)
        error('Nelson:image_processing:sphereRadiusNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'Sphere radius');
      end
      [x, y, z] = meshgrid(-r:r, -r:r, -r:r);
      nhood = (x .^ 2 + y .^ 2 + z .^ 2) <= r ^ 2;
    case 'line'
      if nargin < 2 || nargin > 3
        error('Nelson:image_processing:lineStructuringElementRequiresLengthOptional', '%s structuring element requires %s.', 'Line', 'a length and optional angle');
      end
      len = varargin{2};
      if nargin >= 3
        angle = varargin{3};
      else
        angle = 0;
      end
      len = double(len);
      if ~isscalar(len) || ~isfinite(len) || len < 1 || len ~= floor(len)
        error('Nelson:image_processing:lineLengthPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Line length');
      end
      if ~isscalar(angle) || ~isfinite(double(angle))
        error('Nelson:image_processing:lineAngleFiniteScalar', 'Line angle must be a finite scalar.');
      end
      angle = double(angle) * pi / 180;
      radius = (len - 1) / 2;
      x = round(linspace(-radius * cos(angle), radius * cos(angle), len));
      y = round(linspace(-radius * sin(angle), radius * sin(angle), len));
      rows = max(y) - min(y) + 1;
      cols = max(x) - min(x) + 1;
      nhood = false(rows, cols);
      idx = sub2ind(size(nhood), y - min(y) + 1, x - min(x) + 1);
      nhood(idx) = true;
    case 'disk'
      if nargin < 2 || nargin > 3
        error('Nelson:image_processing:diskStructuringElementRequiresRadiusOptional', '%s structuring element requires %s.', 'Disk', 'a radius and optional decomposition count');
      end
      r = varargin{2};
      if ~isscalar(r) || ~isfinite(double(r)) || r < 0 || r ~= floor(r)
        error('Nelson:image_processing:diskRadiusNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'Disk radius');
      end
      if nargin >= 3 && ~isempty(varargin{3})
        n = varargin{3};
        if ~isscalar(n) || ~isfinite(double(n)) || n ~= floor(n) ...
            || ~(n == 0 || n == 4 || n == 6 || n == 8)
          error('Nelson:image_processing:diskDecompositionCount', 'Disk decomposition count must be 0, 4, 6, or 8.');
        end
      end
      [x, y] = meshgrid(-r:r, -r:r);
      nhood = (x .^ 2 + y .^ 2) <= r ^ 2;
    case 'diamond'
      if nargin ~= 2
        error('Nelson:image_processing:diamondStructuringElementRequiresRadius', '%s structuring element requires %s.', 'Diamond', 'a radius');
      end
      r = varargin{2};
      if ~isscalar(r) || ~isfinite(double(r)) || r < 0 || r ~= floor(r)
        error('Nelson:image_processing:diamondRadiusNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'Diamond radius');
      end
      [x, y] = meshgrid(-r:r, -r:r);
      nhood = (abs(x) + abs(y)) <= r;
    case 'octagon'
      if nargin ~= 2
        error('Nelson:image_processing:octagonStructuringElementRequiresRadius', '%s structuring element requires %s.', 'Octagon', 'a radius');
      end
      r = varargin{2};
      if ~isscalar(r) || ~isfinite(double(r)) || r < 0 || r ~= floor(r) || mod(r, 3) ~= 0
        error('Nelson:image_processing:octagonRadiusNonnegativeMultiple', 'Octagon radius must be a nonnegative multiple of 3.');
      end
      [x, y] = meshgrid(-r:r, -r:r);
      nhood = (abs(x) <= r) & (abs(y) <= r) & (abs(x) + abs(y) <= r + r / 3);
    otherwise
      error('Nelson:image_processing:unsupportedStructuringElementShape', 'Unsupported "%s" option.', 'structuring element shape');
  end
  se = struct('type', shape, 'nhood', logical(nhood));
end
%=============================================================================
