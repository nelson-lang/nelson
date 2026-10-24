%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = light(varargin)
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  if ~isempty(inputArguments) && isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'axes')
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  if rem(length(inputArguments), 2) ~= 0
    error('Nelson:graphics:invalidNameValueArguments', 'Invalid parameter/value pair arguments.');
  end
  h = __light__('Parent', ax, inputArguments{:});
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
