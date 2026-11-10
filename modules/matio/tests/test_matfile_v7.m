%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname();
mkdir(folder);
previous = cd(folder);
cleanup = onCleanup(@() cd(previous));
%=============================================================================
x = magic(4);
s.a = 1;
savemat('v7.mat', 'x', 's', '-v7');
src = fullfile(folder, 'v7.mat');
older = ['''', src, ''' is not a version 7.3 MAT-file: variable ''x'' is loaded whole. ', ...
  'Save with -v7.3 for partial access.'];
%=============================================================================
% whole access needs no warning
lastwarn('');
m = matfile('v7.mat');
asserts.isequal(m.x, x);
asserts.isequal(lastwarn(), '');
%=============================================================================
% blocks load the whole variable with a warning
warning('off', 'Nelson:matfile:wholeVariableLoad');
lastwarn('');
asserts.isequal(m.x(1, 2), 2);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:matfile:wholeVariableLoad');
asserts.isequal(msg, older);
%=============================================================================
% writes keep the file version
m.Properties.Writable = true;
m.y = 3;
m.x(1, 1) = 3;
warning('on', 'Nelson:matfile:wholeVariableLoad');
data = loadmat('v7.mat');
expected = x;
expected(1, 1) = 3;
asserts.isequal(data.x, expected);
asserts.isequal(data.y, 3);
asserts.isequal(data.s, s);
asserts.isequal(sort(who(m)), {'s'; 'x'; 'y'});
%=============================================================================
% a file that is not a MAT-file
fid = fopen('bad.mat', 'w');
fwrite(fid, 'garbage');
fclose(fid);
b = matfile('bad.mat');
asserts.checkerror('b.x', ['''', fullfile(folder, 'bad.mat'), ''' is not a valid MAT-file.']);
%=============================================================================
