%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = message(varargin)
  st = struct();
  st.Identifier = varargin{1};
  st.Arguments = {};
  if nargin > 1
    st.Arguments = varargin(2:end);
  end
  varargout{1} = class(st, 'message');
end
