%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw2 = bwmorph(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  bw = varargin{1};
  ip_assert_full_image(bw, false);
  if ~(ischar(varargin{2}) || isstring(varargin{2}))
    error('Nelson:image_processing:morphologicalOperationString', '%s must be a string.', 'Morphological operation');
  end
  operation = lower(convertStringsToChars(varargin{2}));
  n = 1;
  if nargin == 3
    n = ip_validate_bwmorph_iterations(varargin{3});
  end
  bw2 = ip_to_logical_mask(bw);
  if n == 0
    return
  end
  switch operation
    case {'endpoints', 'branchpoints'}
      bw2 = builtin('__bwmorph_kernel__', bw2, operation, n);
    case {'clean', 'fill', 'majority', 'remove', 'spur', 'bridge', 'thin', 'skel', ...
      'dilate', 'erode', 'open', 'close', 'tophat', 'bothat'}
      bw2 = builtin('__bwmorph_kernel__', bw2, operation, n);
    otherwise
      error('Nelson:image_processing:unsupportedBwmorphOperation', 'Unsupported "%s" option.', 'bwmorph operation');
  end
end
%=============================================================================
