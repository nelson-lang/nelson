%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = num2hex(x)
  narginchk(1, 1);
  if ~(isa(x, 'double') || isa(x, 'single'))
    error('Nelson:elementary_functions:inputClassSingleDouble', 'Input must be of class single or double.');
  end
  x = x(:);
  n = numel(x);
  isSingle = isa(x, 'single');
  rows = cell(max(n, 1), 1);
  for k = 1:n
    if isSingle
      rows{k} = lower(dec2hex(typecast(single(x(k)), 'uint32'), 8));
    else
      rows{k} = lower(dec2hex(typecast(double(x(k)), 'uint64'), 16));
    end
  end
  if n == 0
    s = '';
  else
    s = char(rows);
  end
end
%=============================================================================
