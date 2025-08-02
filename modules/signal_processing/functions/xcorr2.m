%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xcorr2(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  A = varargin{1};
  if nargin == 1
    B = A;
  else
    B = varargin{2};
  end
  varargout{1} = conv2(A, rot90(conj(B), 2));
end
%=============================================================================
