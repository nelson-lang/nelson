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
deleted = [tempname(), '_rmpath_deleted'];
mkdir(deleted);
addpath(deleted);
rmdir(deleted, 's');
asserts.istrue(any(strcmp(strsplit(path(), pathsep), deleted)));
%=============================================================================
% A deleted directory still on the path is removed without warning.
lastwarn('');
rmpath(deleted);
[msg, id] = lastwarn();
asserts.isequal(msg, '');
asserts.isequal(id, '');
asserts.isfalse(any(strcmp(strsplit(path(), pathsep), deleted)));
%=============================================================================
% A directory that is neither on the path nor on disk still warns.
lastwarn('');
rmpath(deleted);
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:rmpath:notADirectory');
%=============================================================================
