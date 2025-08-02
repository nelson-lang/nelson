%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
format('default');
%=============================================================================
D = diag([1 1 1]);
asStr = formattedDisplayText(D);
REF = "     1     0     0
     0     1     0
     0     0     1
";
assert_isequal(asStr, REF);
%=============================================================================
asStr = formattedDisplayText(2 * D + ones(3));
REF = "     3     1     1
     1     3     1
     1     1     3
";
assert_isequal(asStr, REF);
%=============================================================================
st = struct('Type', 'Button', 'Size', 45, 'Enabled', false);
asStr = formattedDisplayText(st, 'UseTrueFalseForLogical', true);
REF = "       Type: 'Button'
       Size: 45
    Enabled: false
";
assert_isequal(asStr, REF);
%=============================================================================
st = struct('Type', 'Button', 'Size', pi, 'Enabled', false);
asStr = formattedDisplayText(st, 'NumericFormat','bank', 'SuppressMarkup', true, 'LineSpacing', 'compact');
REF = "       Type: 'Button'
       Size: 3.14
    Enabled: false
";
assert_isequal(asStr, REF);
%=============================================================================
rng('default');
R = rand(3, 3);
str = formattedDisplayText(R);
REF = "    0.8147    0.9134    0.2785
    0.9058    0.6324    0.5469
    0.1270    0.0975    0.9575
";
assert_isequal(str, REF);
%=============================================================================
str = formattedDisplayText(R, 'NumericFormat', 'bank', 'LineSpacing', 'compact');
REF = "          0.81          0.91          0.28
          0.91          0.63          0.55
          0.13          0.10          0.96
";
assert_isequal(str, REF);
%=============================================================================
