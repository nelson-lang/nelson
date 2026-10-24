%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = bwareaopen(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  bw = ip_validate_binary_image_2d(varargin{1});
  minSize = varargin{2};
  if ~isnumeric(minSize) || ~isreal(minSize) || ~isscalar(minSize) || ~isfinite(double(minSize)) ...
      || minSize < 0 || minSize ~= floor(minSize)
    error('Nelson:image_processing:minimumObjectSizeNonnegativeIntegerScalar', '%s must be a nonnegative integer scalar.', 'Minimum object size');
  end
  if nargin == 3
    cc = bwconncomp(bw, varargin{3});
  else
    cc = bwconncomp(bw);
  end
  out = false(size(bw));
  for k = 1:cc.NumObjects
    if numel(cc.PixelIdxList{k}) >= minSize
      out(cc.PixelIdxList{k}) = true;
    end
  end
end
%=============================================================================
