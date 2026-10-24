%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = ddeset(varargin)
  options = ddeDefaultOptions();
  startIndex = 1;
  while startIndex <= nargin && (isempty(varargin{startIndex}) || isstruct(varargin{startIndex}))
    if isstruct(varargin{startIndex})
      options = ddeMergeOptions(options, varargin{startIndex});
    end
    startIndex = startIndex + 1;
  end
  if startIndex <= nargin
    if mod(nargin - startIndex + 1, 2) ~= 0
      error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
    end
    for k = startIndex:2:nargin
      name = ddeOptionName(varargin{k});
      options.(name) = varargin{k + 1};
    end
  end
end
%=============================================================================
