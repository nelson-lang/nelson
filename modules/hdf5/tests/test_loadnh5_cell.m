%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5load_file = [tempdir(), 'test_h5load_cell.nh5'];
if isfile(test_h5load_file)
  rmfile(test_h5load_file);
end
%=============================================================================
A = {1, 2; 3, 4};
B = {1, A; A, 2};
C = {A, 4; 5, B};
savenh5(test_h5load_file, 'C');
C_REF = C;
clear('A', 'B', 'C');
st = loadnh5(test_h5load_file);
assert_isequal(C_REF, st.C);
%=============================================================================
