%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% `{TypeName.empty}` builds a cell holding the static empty array; the head
% names a class, not a struct variable to expand as a field comma-list.
%=============================================================================
C = {string.empty};
assert_isequal(class(C), 'cell');
assert_isequal(size(C), [1 1]);
assert_isequal(class(C{1}), 'string');
assert_isequal(size(C{1}), [0 0]);
%=============================================================================
C = {double.empty};
assert_isequal(size(C), [1 1]);
assert_isequal(class(C{1}), 'double');
assert_isequal(size(C{1}), [0 0]);
%=============================================================================
C = {int8.empty};
assert_isequal(class(C{1}), 'int8');
assert_isequal(size(C{1}), [0 0]);
%=============================================================================
C = {cell.empty};
assert_isequal(size(C), [1 1]);
assert_isequal(class(C{1}), 'cell');
%=============================================================================
C = {datetime.empty};
assert_isequal(size(C), [1 1]);
assert_isequal(class(C{1}), 'datetime');
assert_isequal(size(C{1}), [0 0]);
%=============================================================================
C = {duration.empty};
assert_isequal(class(C{1}), 'duration');
assert_isequal(size(C{1}), [0 0]);
%=============================================================================
C = {table.empty};
assert_isequal(class(C{1}), 'table');
%=============================================================================
C = {containers.Map};
assert_isequal(class(C{1}), 'containers.Map');
%=============================================================================
C = {string.empty(0, 3)};
assert_isequal(size(C{1}), [0 3]);
%=============================================================================
% struct field comma-lists keep expanding.
s(1).f = 1;
s(2).f = 'a';
C = {s.f};
assert_isequal(C, {1, 'a'});
%=============================================================================
t = struct('f', {});
C = {t.f};
assert_isequal(size(C), [1 0]);
%=============================================================================
eval('u = struct(''f'', {1, 2});');
C = {u.f};
assert_isequal(C, {1, 2});
%=============================================================================
global gcle
gcle = struct('f', {3, 4});
C = {gcle.f};
assert_isequal(C, {3, 4});
clear('global', 'gcle');
%=============================================================================
string = struct('empty', {10, 20});
C = {string.empty};
assert_isequal(C, {10, 20});
clear('string');
%=============================================================================
