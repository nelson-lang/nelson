%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% (6) int2str rounds (half away from zero) instead of truncating.
asserts.isequal(int2str(2.5), '3');
asserts.isequal(int2str(-2.5), '-3');
asserts.isequal(int2str(199.5), '200');
asserts.isequal(int2str(0.5), '1');
asserts.isequal(int2str(-0.5), '-1');
asserts.isequal(int2str(0.49), '0');
asserts.isequal(int2str([2.5 3.5]), '3  4');
%=============================================================================
