%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
REF = struct('b', 3, 'd', 7, 'a', 4);
R = renameStructField(s, 'c', 'd');
asserts.isequal(R, REF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
REF = struct('a', 3, 'b', 7, 'c', 4);
R = renameStructField(s, {'a', 'b', 'c'});
asserts.isequal(R, REF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
REF = struct('c', 3, 'a', 7, 'b', 4);
R = renameStructField(s, {'c', 'a', 'b'});
asserts.isequal(R, REF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
REF = struct('c', 3, 'a', 7, 'b', 4);
R = renameStructField(s, ["c", "a", "b"]);
asserts.isequal(R, REF);
%=============================================================================
s = struct('bla', 3, 'c', 7, 'bli', 4);
REF = struct('blabla', 3, 'c', 7, 'bli', 4);
R = renameStructField(s, 'bla', 'blabla');
asserts.isequal(R, REF);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
msg = _('Number of field names must match number of desired names.');
asserts.checkerror('R = renameStructField(s, ["b"]);', msg);
%=============================================================================
s = struct('b', 3, 'c', 7, 'a', 4);
R = renameStructField(s, ["c", "a", "b"], ["c", "a", "b"]);
asserts.isequal(s, R);
%=============================================================================
Renamed = renameStructField(struct('a', 1, 'd', 4), 'a', 'c');
asserts.istrue(isfield(Renamed, 'c'));
asserts.isfalse(isfield(Renamed, 'a'));
%=============================================================================
