%=============================================================================
% Copyright (c) 2017-2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/741
% <-- Short Description -->
% 'ones' and 'zeros' do not manager logical as typename.
%=============================================================================
R = ones([3 4], 'logical');
REF = cast(ones(3, 4), 'logical');
assert_isequal(R, REF);
%=============================================================================
R = zeros([3 4], 'logical');
REF = cast(zeros(3, 4), 'logical');
assert_isequal(R, REF);
%=============================================================================
R = eye([3 4], 'logical');
REF = cast(eye(3, 4), 'logical');
assert_isequal(R, REF);
%=============================================================================
