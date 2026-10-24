%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Static empty-array constructor for struct, missing and function_handle.
%=============================================================================
R = struct.empty;
assert_istrue(isstruct(R));
assert_isequal(size(R), [0 0]);
assert_isequal(fieldnames(R), cell(0, 1));
assert_isequal(R, struct([]));
%=============================================================================
R = struct.empty(0, 2);
assert_istrue(isstruct(R));
assert_isequal(size(R), [0 2]);
%=============================================================================
R = struct.empty([0 4]);
assert_isequal(size(R), [0 4]);
%=============================================================================
R = [struct.empty; struct()];
assert_isequal(size(R), [1 1]);
%=============================================================================
R = missing.empty;
assert_isequal(class(R), 'missing');
assert_isequal(size(R), [0 0]);
%=============================================================================
R = missing.empty(3, 0);
assert_isequal(class(R), 'missing');
assert_isequal(size(R), [3 0]);
%=============================================================================
R = function_handle.empty;
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 0]);
assert_istrue(isempty(R));
%=============================================================================
R = function_handle.empty(0, 3);
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 3]);
%=============================================================================
R = function_handle.empty(0, 0, 2);
assert_isequal(size(R), [0 0 2]);
%=============================================================================
msg = _('At least one dimension must be zero to create an empty array.');
assert_checkerror('struct.empty(2, 3)', msg);
assert_checkerror('missing.empty(2)', msg);
assert_checkerror('function_handle.empty(2, 3)', msg);
%=============================================================================
C = {struct.empty, function_handle.empty, missing.empty};
assert_isequal(class(C{1}), 'struct');
assert_isequal(class(C{2}), 'function_handle');
assert_isequal(class(C{3}), 'missing');
%=============================================================================
% A local variable named after a type keeps field-access semantics.
struct = cell2struct({7}, {'empty'}, 1);
assert_isequal(struct.empty, 7);
clear('struct');
%=============================================================================
