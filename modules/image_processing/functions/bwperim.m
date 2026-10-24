%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = bwperim(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  bw = ip_validate_binary_image_2d(varargin{1});
  if nargin == 2 && varargin{2} == 4
    se = [0 1 0; 1 1 1; 0 1 0];
  elseif nargin == 1 || varargin{2} == 8
    se = true(3, 3);
  else
    error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
  end
  out = bw & ~imerode(bw, se);
end
%=============================================================================
