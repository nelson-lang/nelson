%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
run([modulepath('string', 'root'), '/examples/parse_measurements.m']);
assert(isequal(measurementNames, ["temperature"; "pressure"; "humidity"]));
assert(max(abs(measurementValues - [21.5; 101.3; 48])) < eps);
assert(isequal(measurementUnits, ["C"; "kPa"; "%"]));
%=============================================================================
