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
function varargout = gram(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  A = varargin{1};
  B = varargin{2};
  sysIn = ss(A, B, [], []);
  option = 'c';
  varargout{1} = gram(sysIn, option);
end
