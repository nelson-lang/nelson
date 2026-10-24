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
asserts.isequal(nargin('isenum'), 1);
asserts.isequal(nargout('isenum'), 1);
%=============================================================================
asserts.isequal(isenum(TestEnumColor.Red), true);
asserts.isequal(isenum(TestEnumColor.Blue), true);
%=============================================================================
asserts.isequal(isenum(3), false);
asserts.isequal(isenum(int32(3)), false);
asserts.isequal(isenum('a'), false);
asserts.isequal(isenum(true), false);
asserts.isequal(isenum({1, 2}), false);
asserts.isequal(isenum([]), false);
asserts.isequal(isenum(single(2)), false);
%=============================================================================
