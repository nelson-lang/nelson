%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/544
% <-- Short Description -->
% add 'folder' fieldname to 'dir' output
%=============================================================================
st = dir(nelsonroot());
names = fieldnames(st);
REF = {'name';
'folder';
'date';
'bytes';
'isdir';
'datenum'};
assert_isequal(names, REF);
assert_isequal(st(1).name, '.')
assert_isequal(st(1).folder, nelsonroot())
assert_isequal(st(1).isdir, true)
%=============================================================================
