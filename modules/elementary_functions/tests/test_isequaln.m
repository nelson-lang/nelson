%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_checkerror('isequaln()', _('Wrong number of input arguments.'));
assert_checkerror('isequaln(1)', _('Wrong number of input arguments.'));
assert_checkerror('isequaln([1, 1])', _('Wrong number of input arguments.'));
%=============================================================================
assert_isequal(nargin('isequaln'), -1);
assert_isequal(nargout('isequaln'), 1);
%=============================================================================
assert_istrue(isequaln(struct([]), struct([])));
%=============================================================================
assert_istrue(isequaln(false, false));
assert_isfalse(isequaln(false, true));
%=============================================================================
assert_istrue(isequaln(single(1), double(1)));
assert_isfalse(isequaln(single(1), single(2)));
assert_istrue(isequaln(single(1), single(1)));
assert_istrue(isequaln(single(ones(3, 3, 3)), single(ones(3, 3, 3))));
%=============================================================================
assert_istrue(isequaln(int8(1), double(1)));
assert_istrue(isequaln(int8(1), int8(1)));
assert_istrue(isequaln(int8(ones(3, 3, 3)), int8(ones(3, 3, 3))));
assert_istrue(isequaln(int16(ones(3, 3, 3)), int16(ones(3, 3, 3))));
assert_istrue(isequaln(int32(ones(3, 3, 3)), int32(ones(3, 3, 3))));
assert_istrue(isequaln(int64(ones(3, 3, 3)), int64(ones(3, 3, 3))));
%=============================================================================
assert_istrue(isequaln(int8(ones(3, 3, 3)), int16(ones(3, 3, 3))));
assert_istrue(isequaln(int16(ones(3, 3, 3)), int32(ones(3, 3, 3))));
assert_istrue(isequaln(int32(ones(3, 3, 3)), int64(ones(3, 3, 3))));
assert_istrue(isequaln(int64(ones(3, 3, 3)), int8(ones(3, 3, 3))));
%=============================================================================
assert_istrue(isequaln(NaN, NaN));
assert_istrue(isequaln(NaN, single(NaN)));
assert_isfalse(isequaln(NaN, Inf));
assert_isfalse(isequaln(NaN, 1.0));
assert_istrue(isequaln([1, 2, NaN, 4], [1, 2, NaN, 4]));
assert_istrue(isequaln(struct('a', NaN, 'b', 2), struct('a', NaN, 'b', 2)));
%=============================================================================
assert_istrue(isequaln(struct('a', NaN, 'b', 2), struct('a', NaN, 'b', 2), struct('a', NaN, 'b', 2)));
%=============================================================================
