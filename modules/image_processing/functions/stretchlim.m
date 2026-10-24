%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function limits = stretchlim(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  ip_assert_full_image(varargin{1}, true);
  img = ip_clip01(ip_to_unit_double(varargin{1}));
  if nargin == 2
    tol = double(varargin{2});
  else
    tol = [0.01 0.99];
  end
  if numel(tol) == 1
    tol = [tol 1 - tol];
  end
  if numel(tol) ~= 2 || any(tol < 0) || any(tol > 1)
    error('Nelson:image_processing:toleranceScalarElementVectorRange', 'Tolerance must be a scalar or a 2-element vector in the range [0, 1].');
  end
  if length(size(img)) == 3
    channels = size(img, 3);
  else
    channels = 1;
  end
  limits = zeros(2, channels);
  if tol(1) >= tol(2)
    limits(2, :) = 1;
    return;
  end
  for c = 1:channels
    if channels == 1
      vals = sort(img(:));
    else
      plane = img(:, :, c);
      vals = sort(plane(:));
    end
    n = numel(vals);
    lo = max(1, min(n, floor(tol(1) * n) + 1));
    hi = max(1, min(n, ceil(tol(2) * n)));
    limits(:, c) = [vals(lo); vals(hi)];
  end
end
%=============================================================================
