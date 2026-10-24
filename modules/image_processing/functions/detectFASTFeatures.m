%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function points = detectFASTFeatures(varargin)
  narginchk(1, 7);
  nargoutchk(0, 1);
  img = ip_corner_prepare_image(varargin{1});
  options = ip_parse_corner_options(varargin(2:nargin));
  metric = ip_fast_corner_metric(img, options.MinContrast);
  points = ip_corner_points_from_metric(metric, options.MinQuality, options.ROI);
end
%=============================================================================
