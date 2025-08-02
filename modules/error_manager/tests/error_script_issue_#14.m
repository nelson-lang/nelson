%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
do_error = false;
try
  A = 1;
  B = [1 1 1];
  C = A / B; % matrix dimensions error
catch
  do_error = true;
  l1 = lasterror()
  lasterror('reset');
  l2 = lasterror()
  assert_isequal(l2.message, '');
  error(l1);
end
assert_istrue(do_error);
%=============================================================================
