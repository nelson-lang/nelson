%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
fd = fopen([path_test, '/nelson_dir_command_display_marker.txt'], 'wt');
fprintf(fd, 'marker');
fclose(fd);
current = pwd();
cd(path_test);
restore = onCleanup(@() cd(current));
%=============================================================================
txt = evalc('dir');
assert_istrue(contains(txt, 'nelson_dir_command_display_marker.txt'));
assert_isfalse(contains(txt, 'struct array with fields'));
%=============================================================================
S = dir;
assert_istrue(isstruct(S));
assert_istrue(any(strcmp({S.name}, 'nelson_dir_command_display_marker.txt')));
%=============================================================================
cd(current);
if isdir(path_test)
  rmdir(path_test, 's');
end
%=============================================================================
