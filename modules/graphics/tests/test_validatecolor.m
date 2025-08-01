%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
R = validatecolor(uint8([128 0 255]));
REF = [0.5020         0    1.0000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
c = uint16([32768 0 65535; 0 65535 0]);
R = validatecolor(c, 'multiple');
REF = [ 0.5000         0    1.0000; 0    1.0000         0];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = validatecolor({'red','green','blue'},'multiple');
REF = eye(3, 3);
assert_isequal(R, REF);
%=============================================================================
R = validatecolor({'#8000FF','#0F0000','#FF9900'}, 'multiple');
REF = [  0.5020    0    1.0000; 0.0588    0  0;1.0000    0.6000    0];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = validatecolor(["#8000FF", "#0F0000", "#FF9900"], 'multiple');
REF = [  0.5020    0    1.0000; 0.0588    0  0;1.0000    0.6000    0];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
