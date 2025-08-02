%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear ans
assert_isfalse(isvar('ans'));
B = 33;
B
assert_isfalse(isvar('ans'));
B = {33};
B
assert_isfalse(isvar('ans'));
33
assert_istrue(isvar('ans'));
clear ans
st.a=1;
st
assert_isfalse(isvar('ans'));
st.a
assert_istrue(isvar('ans'));
assert_isequal(st.a,ans);