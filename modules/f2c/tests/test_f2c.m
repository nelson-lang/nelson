%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TEST_DIR = [tempdir(), 'f2c'];
mkdir(TEST_DIR);
DEST_FILE = [TEST_DIR, '/dgemm.c'];
REF_FILE = [modulepath('f2c','tests'), '/dgemm.c'];
if isfile(DEST_FILE)
  rmfile(DEST_FILE);
end
copyfile([modulepath('f2c','tests'), '/dgemm.f'], [TEST_DIR, '/dgemm.f'])
f2c([TEST_DIR, '/dgemm.f'], TEST_DIR);
DEST_FILE = [TEST_DIR, '/dgemm.c'];
assert_istrue(isfile(DEST_FILE));
R = fileread(DEST_FILE, 'char', 'native');
REF = fileread(REF_FILE,'char', 'native');
assert_isequal(R, REF);
rmfile(DEST_FILE);
%=============================================================================

