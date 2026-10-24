%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = hampel(varargin)
  narginchk(1, 3);
  nargoutchk(0, 4);
  input = varargin{1};
  k = 3;
  nsigma = 3;
  if nargin >= 2
    k = varargin{2};
  end
  if nargin == 3
    nsigma = varargin{3};
  end
  validateInputs(k, nsigma);
  if isvector(input)
    [y, outliers, medians, sigmas] = hampelVector(input, k, nsigma);
  else
    y = zeros(size(input));
    outliers = false(size(input));
    medians = zeros(size(input));
    sigmas = zeros(size(input));
    for col = 1:size(input, 2)
      [y(:, col), outliers(:, col), medians(:, col), sigmas(:, col)] = hampelVector(input(:, col), k, nsigma);
    end
  end
  varargout{1} = y;
  if nargout > 1
    varargout{2} = outliers;
  end
  if nargout > 2
    varargout{3} = medians;
  end
  if nargout > 3
    varargout{4} = sigmas;
  end
end
%=============================================================================
function validateInputs(k, nsigma)
  if ~isscalar(k) || ~isfinite(k) || k ~= fix(k) || k <= 0
    error('Nelson:validators:mustBePositiveInteger', '%s must be a positive integer.', 'Number of neighbors');
  end
  if ~isscalar(nsigma) || ~isnumeric(nsigma) || (~isnan(nsigma) && nsigma < 0)
    error('Nelson:validators:mustBeNonnegative', '%s must be nonnegative.', 'Number of standard deviations');
  end
end
%=============================================================================
function [y, outliers, medians, sigmas] = hampelVector(input, k, nsigma)
  wasRow = size(input, 1) == 1;
  x = input(:);
  y = x;
  outliers = false(size(x));
  medians = zeros(size(x));
  sigmas = zeros(size(x));
  scale = 1.4826022185056;
  for idx = 1:length(x)
    lo = max(1, idx - k);
    hi = min(length(x), idx + k);
    window = x(lo:hi);
    med = signalMedian(window);
    sigma = scale * signalMedian(abs(window - med));
    medians(idx) = med;
    sigmas(idx) = sigma;
    if (sigma > 0 && abs(x(idx) - med) > nsigma * sigma) || (sigma == 0 && x(idx) ~= med)
      y(idx) = med;
      outliers(idx) = true;
    end
  end
  if wasRow
    y = y.';
    outliers = outliers.';
    medians = medians.';
    sigmas = sigmas.';
  end
end
%=============================================================================
