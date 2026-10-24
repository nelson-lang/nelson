%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function setProperties(obj, count, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 0);
  if ~isscalar(count) || count ~= fix(count) || count < 0
    error('Nelson:handle:setProperties:invalidCount', ...
      _('Wrong value for argument #%d: non-negative integer expected.'), 2);
  end
  if count ~= length(varargin)
    error('Nelson:handle:setProperties:invalidCount', ...
      _('Wrong value for argument #%d: expected number of property arguments.'), 2);
  end
  if mod(count, 2) ~= 0
    error('Nelson:handle:setProperties:nameValuePairs', ...
      _('Property arguments must be name-value pairs.'));
  end
  for k = 1:2:count
    name = varargin{k};
    if isstring(name)
      name = char(name);
    end
    if ~ischar(name)
      error('Nelson:handle:setProperties:invalidName', ...
        _('Wrong type for argument #%d: string expected.'), k + 2);
    end
    obj.(name) = varargin{k + 1};
  end
end
%=============================================================================
