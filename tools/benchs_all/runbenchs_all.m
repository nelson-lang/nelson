%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
try
  TESTS_RESULT_DIR = getenv('TESTS_RESULT_DIR');
  if strcmp(TESTS_RESULT_DIR, '') == true
    TESTS_RESULT_DIR = nelsonroot();
  end
  ver_number = version('-number');
  platform_map.('win64') = 'win-x86-64';
  platform_map.('win32') = 'win-x86-32';
  platform_map.('glnxa64') = 'linux-x86-64';
  platform_map.('glnxa32') = 'linux-x86-32';
  platform_map.('maci64') = 'mac-x86-64';
  platform_map.('maci32') = 'mac-x86-32';
  platform_map.('?') = 'unknow-platform';
  platform = platform_map.(computer('arch'));
  filedest = ['benchs_all-', mat2str(ver_number(1)), '.', mat2str(ver_number(2)), '.', mat2str(ver_number(3)), '.', mat2str(ver_number(4)), '-', platform, '.*'];
  r = test_run([], 'benchs', [TESTS_RESULT_DIR, '/', filedest]);
catch
  m = lasterror();
  disp(m.message);
  r = false;
end
exit();
%=============================================================================
