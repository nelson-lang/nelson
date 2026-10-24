%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_assert_label_matrix(labels)
  ip_assert_full_image(labels, false);
  if ~isnumeric(labels)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Label matrix');
  end
  values = double(labels(:));
  if any(~isfinite(values)) || any(values ~= floor(values)) || any(values < 0)
    error('Nelson:image_processing:labelMatrixValuesNonnegativeFiniteIntegers', '%s values must be nonnegative finite integers.', 'Label matrix');
  end
end
%=============================================================================
