%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
root = tempname();
mkdir(root);
src = fullfile(root, 'source.txt');
dst = fullfile(root, 'dest.txt');
fd = fopen(src, 'wt');
fprintf(fd, 'abc');
fclose(fd);
[status, msg] = movefile(src, dst);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isfalse(isfile(src));
asserts.istrue(isfile(dst));
[status, msg] = movefile(fullfile(root, 'missing.txt'), fullfile(root, 'other.txt'));
asserts.isfalse(status);
asserts.istrue(ischar(msg));
rmdir(root, 's');
%=============================================================================
