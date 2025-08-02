%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricyle"];
wheels = [1 2 3];
d = dictionary(wheels, names);
msg = _("'table' format not yet implemented.");
assert_checkerror('E = entries(d)', msg);
%=============================================================================
R = entries(d, 'cell');
REF = {1, "Unicycle"; 2, "Bicycle"; 3, "Tricyle"};
assert_isequal(R, REF);
%=============================================================================
R = entries(d, 'struct');
assert_isequal(R(1), struct('Key', 1, 'Value', "Unicycle"));
assert_isequal(R(2), struct('Key', 2, 'Value', "Bicycle"));
assert_isequal(R(3), struct('Key', 3, 'Value', "Tricyle"));
%=============================================================================
