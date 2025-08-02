%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function EE = fun_MException_3(n)
  try
    test1(n);
  catch ME
    EE = ME;
  end
end
%=============================================================================
function ME = test1(n);
  ME = MException('sayHello:inputError','Input must be char.');
  if (n == true)
    throwAsCaller(ME)
  else
    throw(ME)
  end
end
%=============================================================================
