%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = augstate(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  sys = varargin{1};
  [A, B, C, D] = augstate(sys.A, sys.B, sys.C, sys.D);
  sysa = sys;
  sysa.A = A;
  sysa.B = B;
  sysa.C = C;
  sysa.D = D;
  varargout{1} = sysa;
end
