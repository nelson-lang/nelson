%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = fspecial(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  type = lower(convertStringsToChars(varargin{1}));
  if nargin >= 2 && (strcmp(type, 'average') || strcmp(type, 'gaussian') || strcmp(type, 'log'))
    siz = varargin{2};
  else
    siz = [3 3];
  end
  if numel(siz) == 1
    siz = [siz siz];
  end
  if ~isnumeric(siz) || ~isreal(siz) || numel(siz) ~= 2 || any(~isfinite(double(siz(:)))) ...
      || any(siz(:) < 1) || any(siz(:) ~= floor(siz(:)))
    error('Nelson:image_processing:filterSizeContainPositiveIntegers', '%s size must contain positive integers.', 'Filter');
  end
  siz = double(siz);
  switch type
    case 'average'
      h = ones(siz(1), siz(2));
      h = h / sum(h(:));
    case 'disk'
      if nargin >= 2
        radius = varargin{2};
      else
        radius = 5;
      end
      if ~isnumeric(radius) || ~isreal(radius) || ~isscalar(radius) || ~isfinite(double(radius)) ...
          || radius < 0
        error('Nelson:image_processing:diskRadiusNonnegativeFiniteScalar', '%s must be a nonnegative finite scalar.', 'Disk radius');
      end
      radius = double(radius);
      gridRadius = ceil(radius);
      [x, y] = meshgrid(-gridRadius:gridRadius, -gridRadius:gridRadius);
      h = double((x .^ 2 + y .^ 2) <= radius ^ 2);
      h = h / sum(h(:));
    case 'gaussian'
      if nargin >= 3
        sigma = varargin{3};
      else
        sigma = 0.5;
      end
      if ~isnumeric(sigma) || ~isreal(sigma) || ~isscalar(sigma) || ~isfinite(double(sigma)) || sigma <= 0
        error('Nelson:image_processing:sigmaPositiveFiniteScalar', '%s must be a positive finite scalar.', 'Sigma');
      end
      sigma = double(sigma);
      cy = (siz(1) + 1) / 2; cx = (siz(2) + 1) / 2;
      h = zeros(siz(1), siz(2));
      for y = 1:siz(1)
        for x = 1:siz(2)
          h(y, x) = exp(-((y - cy) ^ 2 + (x - cx) ^ 2) / (2 * sigma ^ 2));
        end
      end
      h = h / sum(h(:));
    case 'sobel'
      row1 = [1 2 1];
      row2 = [0 0 0];
      row3 = -row1;
      h = [row1; row2; row3];
    case 'prewitt'
      row1 = [1 1 1];
      row2 = [0 0 0];
      row3 = -row1;
      h = [row1; row2; row3];
    case 'laplacian'
      alpha = 0.2;
      if nargin >= 2
        alpha = varargin{2};
      end
      if ~isnumeric(alpha) || ~isreal(alpha) || ~isscalar(alpha) || ~isfinite(double(alpha)) ...
          || alpha < 0 || alpha > 1
        error('Nelson:image_processing:alphaFiniteScalarRange', '%s must be a finite scalar in the range [%s].', 'Alpha', '0, 1');
      end
      alpha = double(alpha);
      h = zeros(3, 3);
      h(1, 1) = alpha;
      h(1, 2) = 1 - alpha;
      h(1, 3) = alpha;
      h(2, 1) = 1 - alpha;
      h(2, 2) = -4;
      h(2, 3) = 1 - alpha;
      h(3, 1) = alpha;
      h(3, 2) = 1 - alpha;
      h(3, 3) = alpha;
      h = h / (alpha + 1);
    case 'log'
      if nargin < 3
        sigma = 0.5;
      else
        sigma = varargin{3};
      end
      if ~isnumeric(sigma) || ~isreal(sigma) || ~isscalar(sigma) || ~isfinite(double(sigma)) || sigma <= 0
        error('Nelson:image_processing:sigmaPositiveFiniteScalar', '%s must be a positive finite scalar.', 'Sigma');
      end
      sigma = double(sigma);
      g = fspecial('gaussian', siz, sigma);
      [gy, gx] = gradient(g);
      [gyy, ~] = gradient(gy);
      [~, gxx] = gradient(gx);
      h = gxx + gyy;
      h = h - sum(h(:)) / numel(h);
    otherwise
      error('Nelson:image_processing:unsupportedFilterType', 'Unsupported "%s" option.', 'filter type');
  end
end
%=============================================================================
