%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
clear ans
asserts.isfalse(isvar('ans'));
B = 33;
B
asserts.isfalse(isvar('ans'));
B = {33};
B
asserts.isfalse(isvar('ans'));
33
asserts.istrue(isvar('ans'));
clear ans
st.a = 1;
st
asserts.isfalse(isvar('ans'));
st.a
asserts.istrue(isvar('ans'));
asserts.isequal(st.a, ans);
