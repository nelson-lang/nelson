%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('dir'), 1);
asserts.isequal(nargout('dir'), 1);
%=============================================================================
pathTest = tempname();
mkdir(pathTest);
filewrite(fullfile(pathTest, 'marker.txt'), 'Nelson');
S = dir(pathTest);
asserts.istrue(isstruct(S));
asserts.istrue(any(strcmp({S.name}, 'marker.txt')));
asserts.istrue(all(isfield(S, {'name', 'date', 'bytes', 'isdir', 'datenum'})));
%=============================================================================
S = dir(fullfile(pathTest, '*.txt'));
asserts.isequal(length(S), 1);
asserts.isequal(S.name, 'marker.txt');
rmdir(pathTest, 's');
%=============================================================================
