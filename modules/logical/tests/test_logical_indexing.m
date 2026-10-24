%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
run([modulepath('logical', 'root'), '/examples/logical_indexing.m']);
assert(isequal(validMask, logical([1 0 0 1 0 1])));
assert(isequal(validMeasurements, [12.4 18.7 9.5]));
assert(isequal(cleanMeasurements, [12.4 0 0 18.7 0 9.5]));
%=============================================================================
