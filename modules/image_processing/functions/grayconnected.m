%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = grayconnected(varargin)
  narginchk(3, 5);
  nargoutchk(0, 1);
  values = ip_validate_grayconnected_image(varargin{1});
  row = ip_validate_grayconnected_subscript(varargin{2}, size(values, 1), 'Row');
  col = ip_validate_grayconnected_subscript(varargin{3}, size(values, 2), 'Column');
  tolerance = 0.32;
  if nargin >= 4
    tolerance = ip_validate_grayconnected_tolerance(varargin{4});
  end
  conn = 8;
  if nargin == 5
    conn = ip_validate_watershed_connectivity(varargin{5});
  end
  bw = ip_grayconnected_region(values, row, col, tolerance, conn);
end
%=============================================================================
