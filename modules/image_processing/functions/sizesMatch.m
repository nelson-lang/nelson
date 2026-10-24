%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = sizesMatch(ref, img)
  narginchk(2, 2);
  nargoutchk(0, 1);
  [dim, ref] = ip_spatial_ref_dimension(ref);
  if isnumeric(img) || islogical(img) || ischar(img) || iscell(img) || isstruct(img)
    imageSize = size(img);
  else
    error('Nelson:image_processing:imageArray', 'Image must be an array.');
  end
  if numel(imageSize) < dim
    tf = false;
    return
  end
  tf = isequal(double(imageSize(1:dim)), ref.ImageSize);
end
%=============================================================================
