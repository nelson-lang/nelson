%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_assert_morphology_image(img)
  if ~(isnumeric(img) || islogical(img))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Image');
  end
  if issparse(img)
    error('Nelson:image_processing:sparseImagesNotSupported', '%s are not supported.', 'Sparse images');
  end
  if length(size(img)) > 3
    error('Nelson:image_processing:imageDRgbDVolumeArray', 'Image must be a 2-D, RGB, or 3-D volume array.');
  end
end
%=============================================================================
