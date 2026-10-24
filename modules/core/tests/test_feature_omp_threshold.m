%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Element-wise loops only go parallel above 1e5 elements by default:
% smaller arrays are faster (and immune to CPU oversubscription) on one thread.
asserts.isequal(feature('ompThreshold'), 100000);
%=============================================================================
initialValue = feature('ompThreshold');
feature('ompThreshold', 0);
asserts.isequal(feature('ompThreshold'), 0);
A = rand(128, 256);
asserts.isequal(A + 1, 1 + A);
feature('ompThreshold', 5000);
asserts.isequal(feature('ompThreshold'), 5000);
feature('ompThreshold', initialValue);
asserts.isequal(feature('ompThreshold'), initialValue);
%=============================================================================
asserts.istrue(islogical(feature('ompEnabled')));
%=============================================================================
