%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% empty length argument yields an empty column window
asserts.isequal(size(hamming([])), [0 1]);
asserts.isequal(size(hann([])), [0 1]);
asserts.isequal(size(blackman([])), [0 1]);
asserts.isequal(size(bartlett([])), [0 1]);
%=============================================================================
% a zero length argument also yields an empty column window
asserts.isequal(hamming(0), zeros(0, 1));
asserts.isequal(hann(0), zeros(0, 1));
asserts.isequal(blackman(0), zeros(0, 1));
asserts.isequal(bartlett(0), zeros(0, 1));
asserts.isequal(rectwin(0), ones(0, 1));
asserts.isequal(triang(0), zeros(0, 1));
asserts.isequal(gausswin(0), zeros(0, 1));
asserts.isequal(kaiser(0), zeros(0, 1));
asserts.isequal(chebwin(0), zeros(0, 1));
asserts.isequal(tukeywin(0), zeros(0, 1));
asserts.isequal(blackmanharris(0), zeros(0, 1));
%=============================================================================
% a one-sample window is the scalar 1 for every shape
asserts.isequal(hamming(1), 1);
asserts.isequal(hann(1), 1);
asserts.isequal(blackman(1), 1);
asserts.isequal(bartlett(1), 1);
%=============================================================================
% the shortest non-trivial window keeps its closed-form endpoint values
asserts.isapprox(hamming(2), [0.08; 0.08], 1e-12);
asserts.isequal(hann(2), [0; 0]);
asserts.isequal(bartlett(2), [0; 0]);
asserts.istrue(max(abs(blackman(2))) < 1e-10);
%=============================================================================
