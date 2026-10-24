%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A compound assignment on a cell element beyond the current size auto-grows the
% cell, filling the intermediate slots with [], exactly like a plain brace
% assignment a{3} = x. The read half of the get-modify-set must vivify an
% out-of-bounds brace element to [] so the store-back can grow the cell.
%=============================================================================
% Field sub-assignment beyond the end.
d = {1};
d{3}.k = 9;
assert_isequal(numel(d), 3);
assert_isequal(d{1}, 1);
assert_istrue(isempty(d{2}));
assert_istrue(isstruct(d{3}));
assert_isequal(d{3}.k, 9);
%=============================================================================
% Field sub-assignment at end + 1.
e = {1};
e{2}.k = 9;
assert_isequal(numel(e), 2);
assert_isequal(e{1}, 1);
assert_isequal(e{2}.k, 9);
%=============================================================================
% Empty-cell vivification keeps working (regression, commit 1b0c1082de).
c = {};
c{1}.k = 9;
assert_isequal(numel(c), 1);
assert_isequal(c{1}.k, 9);
%=============================================================================
% A matrix-valued sub-assignment creates a numeric element and grows the cell.
f = {1};
f{5}(2) = 7;
assert_isequal(numel(f), 5);
assert_isequal(f{5}, [0 7]);
assert_istrue(isempty(f{4}));
%=============================================================================
% A two-dimensional matrix sub-assignment on the vivified element.
g = {1};
g{3}(2, 3) = 7;
assert_isequal(numel(g), 3);
assert_isequal(g{3}, [0 0 0; 0 0 7]);
%=============================================================================
% A cell-valued sub-assignment creates a nested cell element.
h = {1};
h{4}{1} = 'a';
assert_isequal(numel(h), 4);
assert_istrue(iscell(h{4}));
assert_isequal(h{4}{1}, 'a');
%=============================================================================
% A deeper nested field chain builds the intermediate struct.
p = {1};
p{3}.a.b = 5;
assert_isequal(numel(p), 3);
assert_isequal(p{3}.a.b, 5);
%=============================================================================
% A two-dimensional brace subscript beyond the end grows the cell.
q = {1};
q{1, 3}.k = 4;
assert_isequal(size(q), [1 3]);
assert_isequal(q{1, 3}.k, 4);
%=============================================================================
% A field sub-assignment onto an in-bounds empty slot vivifies it.
r = {1, [], 3};
r{2}.k = 8;
assert_isequal(numel(r), 3);
assert_isequal(r{2}.k, 8);
%=============================================================================
% A field sub-assignment onto an in-bounds non-empty non-struct still errors.
assert_checkerror('rr = {1, 2, 3}; rr{2}.k = 8;', ...
    _('Unable to perform assignment because dot indexing is not supported for variables of this type.'));
assert_checkerror('rr = {1, 2, 3}; rr{2}.k = 8;', ...
    _('Unable to perform assignment because dot indexing is not supported for variables of this type.'), 'Nelson:structAssToNonStruct');
%=============================================================================
% A plain out-of-bounds brace read (not part of an assignment) still errors.
assert_checkerror('z = {1}; tmp = z{3};', _('Index exceeds array bounds.'));
%=============================================================================
