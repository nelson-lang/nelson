%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
op = nelson.indexing.IndexingOperation('Paren', {5});
assert_isequal(char(op.Type), 'Paren');
assert_istrue(isa(op.Type, 'nelson.indexing.IndexingOperationType'));
assert_isequal(op.Indices{1}, 5);
%=============================================================================
op = nelson.indexing.IndexingOperation('Brace', {2, 3});
assert_isequal(char(op.Type), 'Brace');
assert_isequal(op.Indices{2}, 3);
%=============================================================================
op = nelson.indexing.IndexingOperation('Dot', {}, 'FieldA');
assert_isequal(char(op.Type), 'Dot');
assert_isequal(op.Name, 'FieldA');
assert_istrue(isempty(op.Indices));
%=============================================================================
% Type may also be given directly as an enumeration member
op = nelson.indexing.IndexingOperation(nelson.indexing.IndexingOperationType.ParenDelete, {1});
assert_isequal(char(op.Type), 'ParenDelete');
%=============================================================================
assert_checkerror('nelson.indexing.IndexingOperation(''Nope'', {1})', ...
  'Unknown indexing operation type.');
%=============================================================================
