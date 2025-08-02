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
  % https://nhigham.com/2020/06/30/what-is-the-hilbert-matrix/
  narginchk(1, 2);
  if nargin < 2
    classname = 'double';
  end
  if (isStringScalar(classname)) || (~strcmp(classname, 'double') && ~strcmp(classname,'single'))
    error('Nelson:hilb:notSupportedClass', _('#2 argument must be ''double'' or ''single''.'));
  end
  j = 1:cast(n(1), classname);
  r = 1 ./ (j' + j - 1);
end
%=============================================================================
