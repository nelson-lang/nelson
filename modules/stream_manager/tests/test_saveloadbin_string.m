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
%=============================================================================
A = ["NelSon";
"is";
"not";
"LensOn"];
savebin([tempdir(), 'test_saveload_string.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_string.bin']);
assert_isequal(A, REF);
%=============================================================================
