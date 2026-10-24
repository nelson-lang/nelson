%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function roi = ip_validate_corner_roi(roi)
  if ~isnumeric(roi) || ~isreal(roi) || numel(roi) ~= 4
    error('Nelson:image_processing:roiNumericElementVector', '%s must be a numeric 4-element vector.', 'ROI');
  end
  roi = double(roi(:).');
  if any(~isfinite(roi))
    error('Nelson:image_processing:roiContainFiniteValues', '%s must contain finite values.', 'ROI');
  end
  if roi(3) < 0 || roi(4) < 0
    error('Nelson:image_processing:roiWidthHeightNonnegative', '%s width and height must be nonnegative.', 'ROI');
  end
end
%=============================================================================
