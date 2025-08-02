%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = invhilb(n, classname)
  % https://nhigham.com/2020/06/30/what-is-the-hilbert-matrix/
  narginchk(1, 2)
  if nargin < 2
    classname = 'double';
  end
  if (isStringScalar(classname)) || (~strcmp(classname,'double') && ~strcmp(classname,'single'))
    error('Nelson:invhilb:notSupportedClass', _('#2 argument must be ''double'' or ''single''.'));
  end
  p = n;
  h = zeros(n, classname);
  for i = 1:n
    r = p * p;
    h(i, i) = r / (2 * i - 1);
    for j = i+1:n
      [h, r] = invh(h, r, n, i, j);
    end
    p = ((n-i) * p * (n+i)) / (i^2);
  end
end
%=============================================================================
function [h, r] = invh(h, r, n, i, j)
  r = -((n - j + 1) * r * (n + j - 1))/(j - 1)^2;
  den = i + j - 1;
  h(i, j) = r / den;
  h(j, i) = r / den;
end
%=============================================================================
