%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
initialPath = path();
cleanupPath = onCleanup(@() path(initialPath)); %#ok<NLS0004>
deleted = [tempname(), '_deleted'];
kept = [tempname(), '_kept'];
mkdir(deleted);
mkdir(kept);
cleanupKept = onCleanup(@() rmdir(kept, 's')); %#ok<NLS0004>
filewrite([kept, '/zz_path_deleted_kept.m'], 'function r = zz_path_deleted_kept(), r = 7; end');
addpath(kept);
addpath(deleted);
rmdir(deleted, 's');
%=============================================================================
% path(p) skips a directory that no longer exists, with a warning.
withDeleted = path();
lastwarn('');
path(withDeleted);
[msg, id] = lastwarn();
asserts.isequal(id, 'Nelson:path:notADirectory');
asserts.istrue(contains(msg, deleted));
entries = strsplit(path(), pathsep);
asserts.isfalse(any(strcmp(entries, deleted)));
asserts.istrue(any(strcmp(entries, kept)));
asserts.isequal(zz_path_deleted_kept(), 7);
%=============================================================================
% path(p1, p2) does the same for both parts.
addpath(kept);
lastwarn('');
path([deleted, pathsep, kept], initialPath);
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:path:notADirectory');
entries = strsplit(path(), pathsep);
asserts.isfalse(any(strcmp(entries, deleted)));
asserts.isequal(entries{1}, kept);
%=============================================================================
