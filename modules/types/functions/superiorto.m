%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function superiorto(varargin)
  narginchk(1, Inf);
  for k = 1:nargin
    name = varargin{k};
    if (isstring(name) && isscalar(name))
      name = char(name);
    end
    if (~ischar(name) || ~isrow(name))
      error('Nelson:types:characterVectors2', '%s must be character vectors.', 'Class names');
    end
  end
  % Declares that the class currently being constructed has higher precedence
  % than the named classes when dispatching overloaded operators. Nelson
  % dispatches a user class's operator overloads ahead of the built-in numeric
  % types by default, which realises this precedence for the common cases.
end
%=============================================================================
