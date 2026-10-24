%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = hilb(n, classname)
  narginchk(1, 2);
  if nargin < 2
    outputClass = 'double';
  else
    outputClass = classname;
  end
  if (isStringScalar(outputClass)) || (~strcmp(outputClass, 'double') && ~strcmp(outputClass, 'single'))
    error('Nelson:elementary_functions:notSupportedClass', '#%d argument must be ''%s'' or ''%s''.', 2, 'double', 'single');
  end
  index = 1:cast(n(1), outputClass);
  r = 1 ./ (index.' + index - 1);
end
%=============================================================================
