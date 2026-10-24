%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
run([modulepath('trigonometric_functions', 'root'), '/examples/coordinate_transforms.m']);
assert(polarRoundTripError < 1e-12);
assert(sphericalRoundTripError < 1e-12);
%=============================================================================
