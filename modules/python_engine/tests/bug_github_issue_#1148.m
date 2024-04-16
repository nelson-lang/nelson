%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/1148
% <-- Short Description -->
% pyrun('print(A)','A','A',string(NaN)) did not return expected value.
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
R = pyrun('print(A)','A','A',string(NaN));
assert_isequal(class(R), 'py.NoneType')
assert_isequal(R.char, 'None')
%=============================================================================
