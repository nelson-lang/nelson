%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'), 'path'), '/mat/'];
%=============================================================================
loadmat([mat_dir, 'test_function_handle.mat']);
asserts.isequal(a, -3.9);
asserts.isequal(b, 52);
asserts.isequal(c, 0);
asserts.isequal(parabola, struct());
asserts.isequal(nCf, struct());
asserts.isequal(sqr, struct());
asserts.isequal(lastwarn(), sprintf(_('Cannot read variable: %s'), 'nCf'));
%=============================================================================
