%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function H = hadamard(varargin)
  % https://en.wikipedia.org/wiki/Hadamard_matrix
  % https://mathworld.wolfram.com/HadamardMatrix.html
  narginchk(1, 2);
  nargoutchk(0, 1);
  n = varargin{1};
  if ~isscalar(n)
    error(_('Wrong size for argument #1: scalar expected.'));
  end
  if ~isfinite(n)
    error(_('Wrong value for argument #1: finite value expected.'));
  end
  if ~isreal(n)
    error(_('Wrong value for argument #1: real value expected.'));
  end
  
  classname = 'double';    
  if nargin == 2
    classname = varargin{2};
  end
  if isStringScalar(classname)
    classname = convertStringsToChars(classname);
  end
  N = [n, n/12, n/20];
  [F, E] = log2(N);
  K = find(E > 0 & F == 1/2);
  if isempty(K)
    error(_('N must be 2^k*p, with p = 1, 12, 20.'));
  end
  E = E(K)-1;
  switch K
    case 1
      H = h1(classname);
    case 2
      H = h12(classname);
    case 3
      H = h20(classname);
    otherwise
      error(_('N must be 2^k*p, with p = 1, 12, 20.'));
    end
    l = length(1:E);
    k = 0;
    while k < l
      H = [H  H; H -H];
      k = k + 1;
    end
  end
  %=============================================================================
function H = h1(classname)
  H = ones(classname);
end
%=============================================================================
function H = h12(classname)
  n = 12;
  ti = ones(1, n, classname);
  tu = ones(n - 1,1,classname);
  T = toeplitz([-1 -1 1 -1 -1 -1 1 1 1 -1 1],[-1 1 -1 1 1 1 -1 -1 -1 1 -1]);    
  H = [ti; tu, T];
end
%=============================================================================
function H = h20(classname)
  n = 20;
  ti = ones(1, n, classname);
  tu = ones(n - 1,1,classname);
  hkc = [-1 -1 1 1 -1 -1 -1 -1 1 -1 1 -1 1 1 1 1 -1 -1 1];
  hkr = [1 -1 -1 1 1 -1 -1 -1 -1 1 -1 1 -1 1 1 1 1 -1 -1];
  hk  = hankel(hkc, hkr);
  H = [ti; tu, hk];
end
%=============================================================================
