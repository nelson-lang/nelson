%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
root = [tempdir(), createGUID()];
mkdir(root);
mkdir(fullfile(root, 'a'));
mkdir(fullfile(root, '.hidden'));
mkdir(fullfile(root, 'private'));
mkdir(fullfile(root, '@cls'));
mkdir(fullfile(root, '+pkg'));
mkdir(fullfile(root, 'resources'));
cleanup = onCleanup(@() rmdir(root, 's')); %#ok<NASGU>
p = genpath(root);
asserts.istrue(~isempty(strfind(p, [fullpath(root), pathsep()])));
asserts.istrue(~isempty(strfind(p, [fullfile(fullpath(root), 'a'), pathsep()])));
asserts.istrue(~isempty(strfind(p, [fullfile(fullpath(root), '.hidden'), pathsep()])));
asserts.istrue(isempty(strfind(p, [fullfile(fullpath(root), 'private'), pathsep()])));
asserts.istrue(isempty(strfind(p, [fullfile(fullpath(root), '@cls'), pathsep()])));
asserts.istrue(isempty(strfind(p, [fullfile(fullpath(root), '+pkg'), pathsep()])));
asserts.istrue(isempty(strfind(p, [fullfile(fullpath(root), 'resources'), pathsep()])));
asserts.isequal(genpath([root, '_missing']), '');
asserts.checkerror('genpath(1)', _('Input must be a character vector or string scalar.'));
%=============================================================================
