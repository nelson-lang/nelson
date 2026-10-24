%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = minreal(varargin)
  % Minimal realization and pole-zero cancellation.
  % sysOut = minreal(sysIn)
  % sysOut = minreal(sysIn, tol)
  narginchk(1, 2);
  nargoutchk(0, 1);
  sysIn = varargin{1};
  if nargin < 2
    reduced = minreal(tf(sysIn));
  else
    reduced = minreal(tf(sysIn), varargin{2});
  end
  varargout{1} = zpk(reduced);
end
%=============================================================================
