%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% assignment-suppression flag '*' in fscanf conversion specifiers.
%=============================================================================
dst = [tempdir, 'test_fscanf_suppression.txt'];
fw = fopen(dst, 'wt');
fprintf(fw, '10 20 30 40 50 60');
fclose(fw);
%=============================================================================
fd = fopen(dst, 'r');
[R, C] = fscanf(fd, '%*d %d');
fclose(fd);
asserts.isequal(R, [20; 40; 60]);
asserts.isequal(C, 3);
%=============================================================================
fd = fopen(dst, 'r');
[R, C] = fscanf(fd, '%d %*d');
fclose(fd);
asserts.isequal(R, [10; 30; 50]);
asserts.isequal(C, 3);
%=============================================================================
fd = fopen(dst, 'r');
R = fscanf(fd, '%*2d%d');
fclose(fd);
asserts.isequal(R, [20; 40; 60]);
%=============================================================================
dst2 = [tempdir, 'test_fscanf_suppression2.txt'];
fw = fopen(dst2, 'wt');
fprintf(fw, 'alpha 5 beta 6 gamma 7');
fclose(fw);
%=============================================================================
fd = fopen(dst2, 'r');
[R, C] = fscanf(fd, '%*s %d');
fclose(fd);
asserts.isequal(R, [5; 6; 7]);
asserts.isequal(C, 3);
%=============================================================================
% ordinary fscanf is unaffected.
fd = fopen(dst, 'r');
R = fscanf(fd, '%d');
fclose(fd);
asserts.isequal(R, [10; 20; 30; 40; 50; 60]);
%=============================================================================
