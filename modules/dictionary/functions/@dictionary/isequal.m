%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isequal(varargin)
  narginchk(2, 1000);
  nargoutchk(0, 1);
  varargout{1} = isequalCommon(varargin{:});
end
%=============================================================================
