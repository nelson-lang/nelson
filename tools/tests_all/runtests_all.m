%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ver_number = version('-number');
platform_map.('woa64') = 'win-arm64';
platform_map.('win64') = 'win-x86-64';
platform_map.('win32') = 'win-x86-32';
platform_map.('glnxa64') = 'linux-x86-64';
platform_map.('glnxa32') = 'linux-x86-32';
platform_map.('maci64') = 'mac-x86-64';
platform_map.('maci32') = 'mac-x86-32';
platform_map.('?') = 'unknow-platform';
platform = platform_map.(computer('arch'));
TESTS_RESULT_DIR = getenv('TESTS_RESULT_DIR');
if strcmp(TESTS_RESULT_DIR, '') == true
  TESTS_RESULT_DIR = nelsonroot();
end
stop_on_fail = [];
filedest = ['tests_all-', mat2str(ver_number(1)), '.', mat2str(ver_number(2)), '.', mat2str(ver_number(3)), '.', mat2str(ver_number(4)), '-', platform, '.*'];
try
  r = test_run([], 'all_tests', [TESTS_RESULT_DIR, '/', filedest], stop_on_fail);
catch
  m = lasterror();
  disp(m);
  r = false;
end
if ispc()
  if r == false
    exit(1);
  else
    exit();
  end
else
  exit();
end
%=============================================================================
