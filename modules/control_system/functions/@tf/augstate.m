%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = augstate(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  sys = ss(varargin{1});
  sysa = augstate(sys);
  sysa = tf(sysa);
end
