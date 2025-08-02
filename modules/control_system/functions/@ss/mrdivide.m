%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mrdivide(varargin)
  sys1 = ss(varargin{1});
  sys2 = ss(varargin{2});
  varargout{1} = sys1 * inv(sys2);
end
%=============================================================================
