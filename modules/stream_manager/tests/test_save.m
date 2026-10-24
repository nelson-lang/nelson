%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
folder = tempname();
mkdir(folder);
filename = fullfile(folder, 'save_direct.nh5');
%=============================================================================
A = eye(3);
B = [true false true];
save(filename, 'A', 'B');
%=============================================================================
asserts.istrue(isfile(filename));
st = load(filename);
asserts.isequal(st.A, A);
asserts.isequal(st.B, B);
%=============================================================================
rmfile(filename);
rmdir(folder);
%=============================================================================
