%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Static empty-array constructor `TypeName.empty` for builtin types.
%=============================================================================
R = double.empty;
assert_isequal(class(R), 'double');
assert_isequal(size(R), [0 0]);
assert_istrue(isempty(R));
%=============================================================================
R = single.empty;
assert_isequal(class(R), 'single');
assert_isequal(size(R), [0 0]);
assert_istrue(isempty(R));
%=============================================================================
R = int8.empty;
assert_isequal(class(R), 'int8');
assert_istrue(isempty(R));
%=============================================================================
R = int16.empty;
assert_isequal(class(R), 'int16');
assert_istrue(isempty(R));
%=============================================================================
R = int32.empty;
assert_isequal(class(R), 'int32');
assert_istrue(isempty(R));
%=============================================================================
R = int64.empty;
assert_isequal(class(R), 'int64');
assert_istrue(isempty(R));
%=============================================================================
R = uint8.empty;
assert_isequal(class(R), 'uint8');
assert_istrue(isempty(R));
%=============================================================================
R = uint16.empty;
assert_isequal(class(R), 'uint16');
assert_istrue(isempty(R));
%=============================================================================
R = uint32.empty;
assert_isequal(class(R), 'uint32');
assert_istrue(isempty(R));
%=============================================================================
R = uint64.empty;
assert_isequal(class(R), 'uint64');
assert_istrue(isempty(R));
%=============================================================================
R = logical.empty;
assert_isequal(class(R), 'logical');
assert_isequal(size(R), [0 0]);
assert_istrue(isempty(R));
%=============================================================================
R = char.empty;
assert_isequal(class(R), 'char');
assert_isequal(size(R), [0 0]);
assert_istrue(isempty(R));
%=============================================================================
R = string.empty;
assert_isequal(class(R), 'string');
assert_isequal(size(R), [0 0]);
assert_istrue(isempty(R));
%=============================================================================
R = cell.empty;
assert_isequal(class(R), 'cell');
assert_isequal(size(R), [0 0]);
assert_istrue(isempty(R));
assert_istrue(iscell(R));
%=============================================================================
% Requested-size forms.
R = double.empty(0, 3);
assert_isequal(class(R), 'double');
assert_isequal(size(R), [0 3]);
assert_istrue(isempty(R));
%=============================================================================
R = double.empty(3, 0);
assert_isequal(size(R), [3 0]);
assert_istrue(isempty(R));
%=============================================================================
R = double.empty([0 3]);
assert_isequal(size(R), [0 3]);
%=============================================================================
R = int8.empty(0, 3);
assert_isequal(class(R), 'int8');
assert_isequal(size(R), [0 3]);
%=============================================================================
R = string.empty(0, 3);
assert_isequal(class(R), 'string');
assert_isequal(size(R), [0 3]);
%=============================================================================
R = cell.empty(3, 0);
assert_isequal(class(R), 'cell');
assert_isequal(size(R), [3 0]);
%=============================================================================
% Value equivalence with the ordinary empty constructors.
assert_isequal(double.empty, []);
assert_isequal(cell.empty, {});
assert_isequal(string.empty, strings(0, 0));
%=============================================================================
% A local variable named after a type keeps field-access semantics.
double = struct('empty', 42);
assert_isequal(double.empty, 42);
clear('double');
%=============================================================================
% At least one dimension must be zero.
assert_checkerror('double.empty(2, 3)', _('At least one dimension must be zero to create an empty array.'));
%=============================================================================
