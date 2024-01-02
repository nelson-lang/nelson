%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('exit'), 2);
assert_isequal(nargout('exit'), 0);
%=============================================================================
if ~ispc()
  assert_checkerror('exit(300)', _('Value between 0 and 255 expected.'))
end
%=============================================================================
binpath = modulepath('nelson', 'bin');
if ispc()
  nelson_exe = ['"', binpath, '/nelson-cli', '"'];
else
  nelson_exe = [binpath, '/nelson-cli'];
end
%=============================================================================
R = system([nelson_exe, ' -e "exit(44)"']);
assert_isequal(R, 44);
%=============================================================================
if ispc()
  R = system([nelson_exe, ' -e "exit(447)"']);
  assert_isequal(R, 447);
end
%=============================================================================
cmd = [nelsonroot(), '/modules/core/tests/test_exit/script_exit.m']
[R, msg] = system([nelson_exe, ' -f "' , cmd, '"']);
assert_isequal(R, 33)
%=============================================================================
cmd = [nelson_exe, ' -e "exit(88,''force'')"']
[R, msg] = system(cmd);
assert_isequal(R, 88);
%=============================================================================
