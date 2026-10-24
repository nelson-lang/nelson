%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% eps of non-finite values is NaN
assert(isnan(eps(Inf)));
assert(isnan(eps(-Inf)));
assert(isnan(eps(NaN)));
R = eps(single(Inf));
assert(isnan(R));
asserts.isequal(class(R), 'single');
R = eps(single(NaN));
assert(isnan(R));
% unchanged behaviors
asserts.isequal(eps(1), eps);
asserts.isequal(eps(0), 4.9406564584124654e-324);
asserts.isequal(eps(2), 2 * eps);
asserts.isequal(eps(-1), eps);
%=============================================================================
