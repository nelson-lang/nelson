%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = nansum(x, varargin)
  narginchk(1, inf);
  nargoutchk(0, 1);
  if nargin == 2 && isnumeric(varargin{1}) && ~isscalar(varargin{1})
    % Vector of dimensions: reduce along each one (sum is associative).
    x(isnan(x)) = 0;
    y = x;
    for d = unique(double(varargin{1}(:).'))
      y = sum(y, d);
    end
    return
  end
  y = sum(x, varargin{:}, 'omitnan');
end
%=============================================================================
