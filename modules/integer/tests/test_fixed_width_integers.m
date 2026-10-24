%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
run([modulepath('integer', 'root'), '/examples/fixed_width_integers.m']);
assert(isequal(byteSamples, uint8([0 0 42 255])));
assert(isequal(brightenedSamples, uint8([40 40 82 255])));
assert(isequal(signedRange, [intmin('int16'), intmax('int16')]));
%=============================================================================
