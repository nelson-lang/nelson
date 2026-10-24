%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tests_dir = run_impl_module_tests_dir(moduleName)
  try
    tests_dir = [modulepath(moduleName), '/tests/'];
  catch
    tests_dir = [nelsonroot(), '/modules/', moduleName, '/tests/'];
  end
end
%=============================================================================
