%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = readvars(varargin)
  narginchk(1, inf);
  T = readtable(varargin{:});
  nvars = numel(T.Properties.VariableNames);
  n = nargout;
  if (n == 0)
    n = 1;
  end
  if (n > nvars)
    error('Nelson:tooManyOutputs', 'Too many output arguments.');
  end
  varargout = cell(1, n);
  for k = 1:n
    varargout{k} = T{:, k};
  end
end
%=============================================================================
