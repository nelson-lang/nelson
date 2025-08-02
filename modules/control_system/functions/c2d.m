%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = c2d(varargin)
  narginchk(3, 3);
  nargoutchk(0, 2);
  
  A = varargin{1};
  B = varargin{2};
  T = varargin{3};
  
  mustBeNumeric(A, 1);
  mustBeNumeric(B, 2);
  mustBeNumeric(T, 3);
  
  msg = abcdchk(A, B);
  if ~isempty(msg)
    error(msg.id, msg.message);
  end
  [mA, nA] = size(A);
  [mB, nB] = size(B);
  S = expm([[A, B] * T; zeros(nB, mB + nB)]);
  varargout{1} = S(1:nA, 1:nA);
  if nargout > 1
    varargout{2} = S(1:nA, nA + 1 : nA + nB);
  end
end
