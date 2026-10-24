%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Demonstrate bounded conversion and saturating integer arithmetic.
rawSamples = [-20 0 42 260];
byteSamples = uint8(rawSamples);
brightenedSamples = byteSamples + uint8(40);
signedRange = [intmin('int16'), intmax('int16')];

disp('Converted bytes:');
disp(byteSamples);
disp('Brightened bytes:');
disp(brightenedSamples);
disp('Signed 16-bit range:');
disp(signedRange);
