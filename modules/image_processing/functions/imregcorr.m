%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tform, peakcorr] = imregcorr(varargin)
  narginchk(2, 3);
  nargoutchk(0, 2);
  moving = ip_imregcorr_prepare_image(varargin{1});
  fixed = ip_imregcorr_prepare_image(varargin{2});
  if ~isequal(size(moving), size(fixed))
    error('Nelson:image_processing:movingFixedImagesSameSize', '%s must have the same size.', 'Moving and fixed images');
  end
  transformType = 'translation';
  if nargin == 3
    if ~(ischar(varargin{3}) || isStringScalar(varargin{3}))
      error('Nelson:image_processing:registrationTransformationTypeString', '%s must be a string.', 'Registration transformation type');
    end
    transformType = lower(convertStringsToChars(varargin{3}));
  end
  switch transformType
    case {'translation', 'rigid', 'similarity', 'affine'}
      [dx, dy, peakcorr] = ip_phase_correlation_translation(moving, fixed);
    otherwise
      error('Nelson:image_processing:unsupportedRegistrationTransformationType', 'Unsupported "%s" option.', 'registration transformation type');
  end
  tform = affine2d([1 0 0; 0 1 0; dx dy 1]);
end
%=============================================================================
