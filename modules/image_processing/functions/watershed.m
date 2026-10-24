%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function labels = watershed(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  img = varargin{1};
  if ~(isnumeric(img) || islogical(img))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Image');
  end
  if issparse(img)
    error('Nelson:image_processing:sparseImagesNotSupported', '%s are not supported.', 'Sparse images');
  end
  isVolume = length(size(img)) == 3;
  if length(size(img)) > 3
    error('Nelson:image_processing:imageDDArray', '%s must be a 2-D or 3-D array.', 'Image');
  end
  if ~isreal(img)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Image');
  end
  d = double(img);
  if any(~isfinite(d(:)))
    error('Nelson:image_processing:imageValuesFinite', '%s values must be finite.', 'Image');
  end
  conn = 8;
  if isVolume
    conn = 26;
  end
  if nargin == 2
    conn = ip_validate_watershed_connectivity(varargin{2}, isVolume);
  end
  labels = builtin('__watershed_kernel__', img, conn);
end
%=============================================================================
