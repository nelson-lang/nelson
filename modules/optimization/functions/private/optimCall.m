%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = optimCall(fun, varargin)
  if isstring(fun)
    fun = char(fun);
  end
  if ischar(fun)
    fun = str2func(fun);
  end
  if ~isa(fun, 'function_handle')
    error('Nelson:optimization:invalidObjective', 'Objective must be a function handle or a function name.');
  end
  [varargout{1:nargout}] = fun(varargin{:});
end
%=============================================================================
