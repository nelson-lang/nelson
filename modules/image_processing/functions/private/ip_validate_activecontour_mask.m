%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_validate_activecontour_mask(mask, imageSize)
  if ~(isnumeric(mask) || islogical(mask))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Initial mask');
  end
  if issparse(mask)
    error('Nelson:image_processing:sparseMasksNotSupported', '%s are not supported.', 'Sparse masks');
  end
  if ~isreal(mask)
    error('Nelson:validators:mustBeReal', '%s must be real.', 'Initial mask');
  end
  if ~isequal(size(mask), imageSize)
    error('Nelson:image_processing:initialMaskSameSizeImage', '%s must have the same size as %s.', 'Initial mask', 'the image');
  end
  d = double(mask);
  if any(~isfinite(d(:)))
    error('Nelson:image_processing:initialMaskContainFiniteValues', '%s must contain finite values.', 'Initial mask');
  end
  values = unique(d(:));
  if any(values ~= 0 & values ~= 1)
    error('Nelson:image_processing:initialMaskContainBinaryValues', '%s must contain binary values.', 'Initial mask');
  end
end
%=============================================================================
