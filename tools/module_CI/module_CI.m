%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
MODULE_GIT_URL = getenv('MODULE_GIT_URL');
MODULE_NAME = getenv('MODULE_NAME');
DESTINATION_DIRECTORY = getenv('DESTINATION_DIRECTORY');
%=============================================================================
arch_name = computer('arch');
%=============================================================================
try
  nmm('install', MODULE_GIT_URL);
catch
  disp(getLastReport());
  exit(1);
end
%=============================================================================
try
  nmm('package', MODULE_NAME, DESTINATION_DIRECTORY);
catch
  disp(getLastReport());
  exit(1);
end
%=============================================================================
try
  test_run(MODULE_NAME, 'all', [DESTINATION_DIRECTORY ,'/tests-', MODULE_NAME, '-', arch_name, '.*']);
catch
  disp(getLastReport());
  exit(1);
end
%=============================================================================
exit(0);
%=============================================================================
