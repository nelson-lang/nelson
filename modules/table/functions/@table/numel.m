%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = numel(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  obj = varargin{1};
  varargout{1} = prod(size(obj));
end
%=============================================================================
