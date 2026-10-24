%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imresize3(varargin)
  narginchk(2, 6);
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
  inputSize = [size(volume, 1), size(volume, 2), size(volume, 3)];
  [outputSize, options] = ip_parse_imresize3_options(varargin(2:end), inputSize);
  out = builtin('__imresize3_kernel__', volume, outputSize, options.Method);
end
%=============================================================================
