%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bytecode_varargin_varargout(varargin)
  varargout{1} = nargin;
  varargout{2} = varargin;
  if nargin >= 1
    varargout{3} = varargin{1};
  else
    varargout{3} = [];
  end
end
%=============================================================================
