%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function files = discover_discoverModuleFiles(moduleName, kind)
  try
    testsDir = [modulepath(moduleName), '/tests/'];
  catch
    testsDir = [nelsonroot(), '/modules/', moduleName, '/tests/'];
  end
  files = discover_discoverDirectoryFiles(testsDir, kind);
end
