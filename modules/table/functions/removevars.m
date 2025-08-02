%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = removevars(varargin)
  nargoutchk(0, 1);
  narginchk(1, 2);
  T = varargin{1};
  mustBeA(T, 'table', 1);
  if (nargin < 2)
    vars = [];
  else
    vars = varargin{2};
  end
  T(:, vars) = [];
  varargout{1} = T;
end
%=============================================================================
