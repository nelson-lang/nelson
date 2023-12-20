%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = d2c(varargin)
  % [a, b] = d2c(phi, gamma, t)
  
  narginchk(3,3);
  nargoutchk(0, 2);
  
  phi = varargin{1};
  gamma = varargin{2};
  T = varargin{3};
  
  [msg, phi, gamma] = abcdchk(phi, gamma);
  if ~isempty(msg)
    error(msg);
  end
  
  [m, n] = size(phi);
  [m, nb] = size(gamma);
  
  if m == 1 && phi == 1
    a = 0;
    b = gamma / T;
    return
  end
  
  b = zeros(m, nb);
  nz = 0;
  nonzero = [];
  for i=1:nb
    if (any(gamma(:,i ) ~= 0)) 
      nonzero = [nonzero, i];
      nz = nz + 1;
    end
  end
  
  m = [[phi, gamma(:, nonzero)]; zeros(nz, n), eye(nz)];
  [s, dummy] = logm(m);
  s = real(s / T);
  a = s(1:n, 1:n);
  if length(b)
    b(:, nonzero) = s(1:n, n + 1 : n + nz);
  end
end
