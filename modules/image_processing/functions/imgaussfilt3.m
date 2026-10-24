%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imgaussfilt3(varargin)
  narginchk(1, 8);
  nargoutchk(0, 1);
  volume = varargin{1};
  if ~(isnumeric(volume) || islogical(volume))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Volume');
  end
  if issparse(volume)
    error('Nelson:image_processing:sparseVolumesNotSupported', '%s are not supported.', 'Sparse volumes');
  end
  if length(size(volume)) ~= 3
    error('Nelson:image_processing:volumeDArray', '%s must be a 3-D array.', 'Volume');
  end
  options = ip_parse_imgaussfilt3_options(varargin(2:end));
  out = builtin('__imgaussfilt3_kernel__', volume, options.FilterSize, options.Sigma, options.Padding);
end
%=============================================================================
