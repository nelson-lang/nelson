%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = hsvd(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  sys = varargin{1};
  % Get gramians
  P = gram(sys, 'c');
  Q = gram(sys, 'o');
  % Get hankel singular values
  hsvd = sqrt(eig(P * Q));
  varargout{1} = hsvd;
end
%=============================================================================
