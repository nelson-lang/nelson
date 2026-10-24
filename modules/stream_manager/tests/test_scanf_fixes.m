%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a format that consumes nothing never loops: nothing is converted
asserts.isequal(size(sscanf('abc', '')), [0 0]);
asserts.isequal(size(sscanf('1 2 3', '')), [0 0]);
asserts.isequal(size(sscanf('', '')), [0 0]);
asserts.isequal(size(sscanf('abc', 'x')), [0 0]);
asserts.isequal(size(sscanf('abc', 'abc')), [0 0]);
asserts.isequal(size(sscanf('1 2 3', 'a')), [0 0]);
%=============================================================================
% the same holds for fscanf
f = [tempname() '.txt'];
fid = fopen(f, 'w');
fwrite(fid, '1 2 3');
fclose(fid);
fid = fopen(f, 'r');
R = fscanf(fid, '');
fclose(fid);
delete(f);
asserts.isequal(size(R), [0 0]);
%=============================================================================
