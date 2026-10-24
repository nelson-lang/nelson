%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
probe_dir = tempname();
mkdir(probe_dir);
c = onCleanup(@() rmdir(probe_dir, 's')); %#ok<NASGU>
%=============================================================================
% delete(filename) removes a file
f1 = fullfile(probe_dir, 'nls_delete_probe_1.txt');
filewrite(f1, 'one');
asserts.istrue(isfile(f1));
delete(f1);
asserts.isfalse(isfile(f1));
%=============================================================================
% string scalar input
f2 = fullfile(probe_dir, 'nls_delete_probe_2.txt');
filewrite(f2, 'two');
delete(string(f2));
asserts.isfalse(isfile(f2));
%=============================================================================
% wildcards
f3 = fullfile(probe_dir, 'nls_delete_probe_3.txt');
f4 = fullfile(probe_dir, 'nls_delete_probe_4.txt');
filewrite(f3, 'three');
filewrite(f4, 'four');
delete(fullfile(probe_dir, 'nls_delete_probe_*.txt'));
asserts.isfalse(isfile(f3));
asserts.isfalse(isfile(f4));
%=============================================================================
% several filenames in one call
f5 = fullfile(probe_dir, 'nls_delete_probe_5.txt');
f6 = fullfile(probe_dir, 'nls_delete_probe_6.txt');
filewrite(f5, 'five');
filewrite(f6, 'six');
delete(f5, f6);
asserts.isfalse(isfile(f5));
asserts.isfalse(isfile(f6));
%=============================================================================
% missing file warns and does not error
warning('off', 'Nelson:FileNotFound');
delete(fullfile(probe_dir, 'nls_delete_probe_none.txt'));
warning('on', 'Nelson:FileNotFound');
%=============================================================================
% cell array of filenames
f7 = fullfile(probe_dir, 'nls_delete_probe_7.txt');
f8 = fullfile(probe_dir, 'nls_delete_probe_8.txt');
filewrite(f7, 'seven');
filewrite(f8, 'eight');
delete({f7, f8});
asserts.isfalse(isfile(f7));
asserts.isfalse(isfile(f8));
%=============================================================================
% string array of filenames, wildcard per element
f9 = fullfile(probe_dir, 'nls_delete_probe_9.txt');
f10 = fullfile(probe_dir, 'nls_delete_other_10.dat');
f11 = fullfile(probe_dir, 'nls_delete_other_11.dat');
filewrite(f9, 'nine');
filewrite(f10, 'ten');
filewrite(f11, 'eleven');
delete([string(f9); string(fullfile(probe_dir, 'nls_delete_other_*.dat'))]);
asserts.isfalse(isfile(f9));
asserts.isfalse(isfile(f10));
asserts.isfalse(isfile(f11));
%=============================================================================
% a missing element warns and the other files are still removed
f12 = fullfile(probe_dir, 'nls_delete_probe_12.txt');
filewrite(f12, 'twelve');
missing_file = fullfile(probe_dir, 'nls_delete_probe_none.txt');
lastwarn('');
warning('off', 'Nelson:FileNotFound');
delete({missing_file, f12});
warning('on', 'Nelson:FileNotFound');
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:FileNotFound');
asserts.isfalse(isfile(f12));
%=============================================================================
% character vector followed by a cell array
f13 = fullfile(probe_dir, 'nls_delete_probe_13.txt');
f14 = fullfile(probe_dir, 'nls_delete_probe_14.txt');
filewrite(f13, 'thirteen');
filewrite(f14, 'fourteen');
delete(f13, {f14});
asserts.isfalse(isfile(f13));
asserts.isfalse(isfile(f14));
%=============================================================================
% empty vectors delete nothing
delete({});
delete(string([]));
%=============================================================================
% invalid element
asserts.checkerror('delete({1})', _('A valid filename expected.'));
asserts.checkerror('delete({1})', _('A valid filename expected.'), 'Nelson:files_folders_functions:validFilenameExpected');
%=============================================================================
% handle objects are still dispatched to their own delete
m = containers.Map();
delete(m);
asserts.isfalse(isvalid(m));
%=============================================================================
