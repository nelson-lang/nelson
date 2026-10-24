%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
try
  run([modulepath('dynamic_link'), '/examples/call_fortran.m']);
catch e
  if contains(e.message, 'BLAS library not found')
    skip_testsuite(true, 'BLAS library not available on this system.');
  else
    rethrow(e);
  end
end
%=============================================================================