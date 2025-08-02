%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/loadsavebin']);
F = [nelsonroot(), '/modules/stream_manager/tests/test_loadbin.bin'];
REF_A = ones(5, 6);
REF_B = 'A string saved.';
REF_C = single(REF_A);
REF_D = int8(REF_A);
clear('A', 'B', 'C', 'D');
loadbin(F, 'A', 'B', 'C', 'D');
assert_isequal(A, REF_A);
assert_isequal(B, REF_B);
assert_isequal(C, REF_C);
assert_isequal(D, REF_D);
%=============================================================================
