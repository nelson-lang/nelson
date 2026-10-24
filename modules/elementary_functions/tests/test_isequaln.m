%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.checkerror('isequaln()', _('Not enough input arguments.'));
asserts.checkerror('isequaln(1)', _('Not enough input arguments.'));
asserts.checkerror('isequaln([1, 1])', _('Not enough input arguments.'));
%=============================================================================
asserts.isequal(nargin('isequaln'), -1);
asserts.isequal(nargout('isequaln'), 1);
%=============================================================================
asserts.istrue(isequaln(struct([]), struct([])));
%=============================================================================
asserts.istrue(isequaln(false, false));
asserts.isfalse(isequaln(false, true));
%=============================================================================
asserts.istrue(isequaln(single(1), double(1)));
asserts.isfalse(isequaln(single(1), single(2)));
asserts.istrue(isequaln(single(1), single(1)));
asserts.istrue(isequaln(single(ones(3, 3, 3)), single(ones(3, 3, 3))));
%=============================================================================
asserts.istrue(isequaln(int8(1), double(1)));
asserts.istrue(isequaln(int8(1), int8(1)));
asserts.istrue(isequaln(int8(ones(3, 3, 3)), int8(ones(3, 3, 3))));
asserts.istrue(isequaln(int16(ones(3, 3, 3)), int16(ones(3, 3, 3))));
asserts.istrue(isequaln(int32(ones(3, 3, 3)), int32(ones(3, 3, 3))));
asserts.istrue(isequaln(int64(ones(3, 3, 3)), int64(ones(3, 3, 3))));
%=============================================================================
asserts.istrue(isequaln(int8(ones(3, 3, 3)), int16(ones(3, 3, 3))));
asserts.istrue(isequaln(int16(ones(3, 3, 3)), int32(ones(3, 3, 3))));
asserts.istrue(isequaln(int32(ones(3, 3, 3)), int64(ones(3, 3, 3))));
asserts.istrue(isequaln(int64(ones(3, 3, 3)), int8(ones(3, 3, 3))));
%=============================================================================
asserts.istrue(isequaln(NaN, NaN));
asserts.istrue(isequaln(NaN, single(NaN)));
asserts.isfalse(isequaln(NaN, Inf));
asserts.isfalse(isequaln(NaN, 1.0));
asserts.istrue(isequaln([1, 2, NaN, 4], [1, 2, NaN, 4]));
asserts.istrue(isequaln(struct('a', NaN, 'b', 2), struct('a', NaN, 'b', 2)));
%=============================================================================
asserts.istrue(isequaln(struct('a', NaN, 'b', 2), struct('a', NaN, 'b', 2), struct('a', NaN, 'b', 2)));
%=============================================================================
