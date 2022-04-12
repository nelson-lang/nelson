%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/loadsavebin']);
%=============================================================================
% function_handle
A = str2func('sin');
savebin([tempdir(), 'test_saveload_fh.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_fh.bin']);
assert_isequal(A, REF);
%=============================================================================
