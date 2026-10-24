%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = lasterr(varargin)
  narginchk(0, 2);
  nargoutchk(0, 2);
  previous = lasterror();
  if nargin >= 1
    msg = varargin{1};
    if ~ischar(msg) && ~(isstring(msg) && isscalar(msg))
      error('Nelson:types:firstArgumentNotText', 'First argument must be a character vector or a string scalar.');
    end
    s.message = convertStringsToChars(msg);
    if nargin >= 2
      id = varargin{2};
      if ~ischar(id) && ~(isstring(id) && isscalar(id))
        error('Nelson:types:secondArgumentNotText', 'Second argument must be a character vector or a string scalar.');
      end
      s.identifier = convertStringsToChars(id);
    else
      s.identifier = '';
    end
    s.stack = struct('file', {}, 'name', {}, 'line', {});
    lasterror(s);
  end
  varargout{1} = previous.message;
  if nargout >= 2
    varargout{2} = previous.identifier;
  end
end
%=============================================================================
