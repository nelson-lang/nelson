%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(accumarray([1; 2; 1; 3], [10; 20; 30; 40]), [40; 20; 40]);
%=============================================================================
assert_isequal(accumarray([1 1; 2 2; 1 1], [5; 6; 7]), [12 0; 0 6]);
%=============================================================================
assert_isequal(accumarray([1; 1; 2], [3; 5; 7], [], @max), [5; 7]);
%=============================================================================
assert_isequal(accumarray([1; 3], [9; 9], [4 1]), [9; 0; 9; 0]);
%=============================================================================
assert_isequal(accumarray([1; 2; 3], 5), [5; 5; 5]);
%=============================================================================
assert_isequal(accumarray([1; 3], [9; 9], [4 1], [], -1), [9; -1; 9; -1]);
%=============================================================================
% sparse output requested with the sixth argument (logical or numeric)
R = accumarray([1; 3], [5; 7], [3 1], @sum, 0, true);
assert_istrue(issparse(R));
assert_isequal(full(R), [5; 0; 7]);
%=============================================================================
R = accumarray([1; 3], [5; 7], [3 1], @sum, 0, 1);
assert_istrue(issparse(R));
assert_isequal(full(R), [5; 0; 7]);
%=============================================================================
R = accumarray([1; 3], [5; 7], [3 1], @sum, 0, false);
assert_isfalse(issparse(R));
assert_isequal(R, [5; 0; 7]);
%=============================================================================
% cell output when the accumulation function returns a cell
R = accumarray([1; 2; 1], [1; 2; 3], [], @(x){x}, {});
assert_istrue(iscell(R));
assert_isequal(size(R), [2 1]);
assert_isequal(R{1}, [1; 3]);
assert_isequal(R{2}, 2);
%=============================================================================
% empty groups in cell output hold [] by default ({} fill)
R = accumarray([1; 3], [5; 7], [3 1], @(x){x}, {});
assert_isequal(R{1}, 5);
assert_isequal(R{2}, []);
assert_isequal(R{3}, 7);
%=============================================================================
% a non-empty cell fill supplies the content of untouched cells
R = accumarray([1; 3], [5; 7], [3 1], @(x){x}, {99});
assert_isequal(R{1}, 5);
assert_isequal(R{2}, 99);
assert_isequal(R{3}, 7);
%=============================================================================
% a cell fill with a scalar-returning function keeps a numeric output
R = accumarray([1; 2; 1], [1; 2; 3], [], @(x) sum(x), {});
assert_isfalse(iscell(R));
assert_isequal(R, [4; 2]);
%=============================================================================
% multi-column subscripts with cell output
R = accumarray([1 1; 2 1; 1 1], [1; 2; 3], [2 1], @(x){x}, {});
assert_istrue(iscell(R));
assert_isequal(R{1}, [1; 3]);
assert_isequal(R{2}, 2);
%=============================================================================
