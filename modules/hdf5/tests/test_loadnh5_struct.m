%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_struct.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
A.a = 1;
A.b = uint8(2);
B.c = 4;
B.d = 5;
B.e = A;
A.f = B;
savenh5(test_h5save_file, 'A');
A_REF = A;
clear A;
st = loadnh5(test_h5save_file);
assert_isequal(A_REF, st.A);
%=============================================================================
