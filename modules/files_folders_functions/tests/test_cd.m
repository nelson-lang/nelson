%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TMPDIR = tempdir();
if ismac()
  TMPDIR = ['/private', TMPDIR];
end
%=============================================================================
cd(nelsonroot())
eval(['cd(''', eval('TMPDIR'), ''')']);
R = pwd();
REF = TMPDIR;
assert_isequal([R, '/'], REF);
%=============================================================================
cd(nelsonroot())
eval(['cd            (''', eval('TMPDIR'), ''')']);
R = pwd();
REF = TMPDIR;
assert_isequal([R, '/'], REF);
%=============================================================================
cd(nelsonroot())
eval(['cd         (      ''', eval('TMPDIR'), ''')']);
R = pwd();
REF = TMPDIR;
assert_isequal([R, '/'], REF);
%=============================================================================
cd(nelsonroot())
cd(TMPDIR)
R = pwd();
REF = TMPDIR;
assert_isequal([R, '/'], REF);
%=============================================================================
cd(nelsonroot())
currentDirectory = pwd();
cd .
newDirectory = pwd();
assert_isequal(currentDirectory, newDirectory);
%=============================================================================
cd(nelsonroot())
currentDirectory = pwd();
cd     .
newDirectory = pwd();
assert_isequal(currentDirectory, newDirectory);
%=============================================================================
currentDirectory = pwd();
cd('.')
newDirectory = pwd();
assert_isequal(currentDirectory, newDirectory);
%=============================================================================
