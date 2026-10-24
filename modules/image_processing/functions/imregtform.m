%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tform = imregtform(varargin)
  narginchk(5, 13);
  nargoutchk(0, 1);
  moving = ip_imregcorr_prepare_image(varargin{1});
  fixed = ip_imregcorr_prepare_image(varargin{2});
  if ~isequal(size(moving), size(fixed))
    error('Nelson:image_processing:movingFixedImagesSameSize', '%s must have the same size.', 'Moving and fixed images');
  end
  transformType = ip_registration_transform_type(varargin{3});
  optimizer = ip_registration_optimizer(varargin{4});
  metricType = ip_registration_metric_type(varargin{5});
  options = ip_parse_registration_options(varargin(6:nargin), optimizer);
  T = ip_estimate_registration_tform(moving, fixed, transformType, metricType, options);
  tform = affine2d(T);
end
%=============================================================================
