%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [counts, binLocations] = imhist(varargin)
  narginchk(1, 2);
  nargoutchk(0, 2);
  img = varargin{1};
  ip_assert_full_image(img, false);
  if nargin == 2
    n = varargin{2};
  elseif islogical(img)
    n = 2;
  elseif isa(img, 'uint16')
    n = 65536;
  else
    n = 256;
  end
  n = double(n);
  if numel(n) ~= 1 || ~isfinite(n) || n < 1 || n ~= floor(n)
    error('Nelson:image_processing:numberBinsPositiveIntegerScalar', '%s must be a positive integer scalar.', 'Number of bins');
  end
  switch class(img)
    case {'double', 'single', 'logical'}
      scale = 1;
      offset = 0;
    case {'uint8', 'int8'}
      scale = 255;
      offset = double(isa(img, 'int8')) * 128;
    case {'uint16', 'int16'}
      scale = 65535;
      offset = double(isa(img, 'int16')) * 32768;
    case {'uint32', 'int32'}
      scale = 4294967295;
      offset = double(isa(img, 'int32')) * 2147483648;
    otherwise
      scale = 1;
      offset = 0;
  end
  d = ip_clip01((double(img) + offset) / scale);
  if n == 1
    idx = ones(size(d));
  else
    idx = floor(d * (n - 1)) + 1;
  end
  counts = zeros(n, 1);
  for k = 1:numel(idx)
    counts(idx(k)) = counts(idx(k)) + 1;
  end
  if n == 1
    binLocations = 0;
  else
    binLocations = ((0:n - 1)' / (n - 1)) * scale - offset;
  end
end
%=============================================================================
