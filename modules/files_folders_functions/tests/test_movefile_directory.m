%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: movefile on a directory must never delete the source unless the
% content has been fully moved to the destination (no data loss, no path-join
% corruption).
%=============================================================================
root = tempname();
mkdir(root);
%=============================================================================
% dir -> missing destination: the source directory is renamed to the
% destination (contents end up directly under the destination).
srcA = fullfile(root, 'srcA');
dstA = fullfile(root, 'dstA');
mkdir(srcA);
filewrite(fullfile(srcA, 'a.txt'), 'hello');
[status, msg] = movefile(srcA, dstA);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isfalse(isdir(srcA));
asserts.istrue(isdir(dstA));
asserts.istrue(isfile(fullfile(dstA, 'a.txt')));
asserts.isequal(fileread(fullfile(dstA, 'a.txt')), 'hello');
%=============================================================================
% dir -> existing destination directory: the source directory is placed inside
% the destination (destination/<source name>). Nothing is lost.
srcB = fullfile(root, 'srcB');
dstB = fullfile(root, 'dstB');
mkdir(srcB);
mkdir(dstB);
filewrite(fullfile(srcB, 'b.txt'), 'world');
[status, msg] = movefile(srcB, dstB);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isfalse(isdir(srcB));
asserts.istrue(isdir(dstB));
asserts.istrue(isfile(fullfile(dstB, 'srcB', 'b.txt')));
asserts.isequal(fileread(fullfile(dstB, 'srcB', 'b.txt')), 'world');
%=============================================================================
% nested subdirectory, existing destination: the whole tree is preserved.
srcN = fullfile(root, 'srcN');
dstN = fullfile(root, 'dstN');
mkdir(fullfile(srcN, 'sub'));
filewrite(fullfile(srcN, 'top.txt'), 'top');
filewrite(fullfile(srcN, 'sub', 'deep.txt'), 'deep');
mkdir(dstN);
[status, msg] = movefile(srcN, dstN);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isfalse(isdir(srcN));
asserts.istrue(isfile(fullfile(dstN, 'srcN', 'top.txt')));
asserts.istrue(isfile(fullfile(dstN, 'srcN', 'sub', 'deep.txt')));
asserts.isequal(fileread(fullfile(dstN, 'srcN', 'top.txt')), 'top');
asserts.isequal(fileread(fullfile(dstN, 'srcN', 'sub', 'deep.txt')), 'deep');
%=============================================================================
% single-file control: a plain file move keeps working.
srcF = fullfile(root, 'file_src.txt');
dstF = fullfile(root, 'file_dst.txt');
filewrite(srcF, 'single');
[status, msg] = movefile(srcF, dstF);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isfalse(isfile(srcF));
asserts.istrue(isfile(dstF));
asserts.isequal(fileread(dstF), 'single');
%=============================================================================
% missing source directory: no source is destroyed, an error is reported.
[status, msg] = movefile(fullfile(root, 'does_not_exist'), fullfile(root, 'target'));
asserts.isfalse(status);
asserts.istrue(ischar(msg));
asserts.istrue(~isempty(msg));
%=============================================================================
rmdir(root, 's');
%=============================================================================
