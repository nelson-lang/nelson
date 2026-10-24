%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = bvpset(varargin)
  options = bvpDefaultOptions();
  startIndex = 1;
  if nargin >= 1 && isstruct(varargin{1})
    options = bvpMergeOptions(options, varargin{1});
    startIndex = 2;
  end
  if startIndex <= nargin
    if mod(nargin - startIndex + 1, 2) ~= 0
      error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
    end
    for k = startIndex:2:nargin
      name = bvpOptionName(varargin{k});
      options.(name) = varargin{k + 1};
    end
  end
end
%=============================================================================
