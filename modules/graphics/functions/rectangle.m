%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = rectangle(varargin)
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  [parent, inputArguments] = graphicsParseParent(inputArguments);
  if rem(length(inputArguments), 2) ~= 0
    error('Nelson:graphics:invalidNameValueArguments', 'Invalid parameter/value pair arguments.');
  end
  args = inputArguments;
  if ~isempty(parent)
    args = [{'Parent', parent}, args];
  end
  handle = __rectangle__(args{:});
  if nargout > 0
    varargout{1} = handle;
  end
end
%=============================================================================
