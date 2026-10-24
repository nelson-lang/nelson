%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('string'), 1);
asserts.isequal(nargout('string'), 1);
%=============================================================================
R = string({1, 2; 3, 4});
REF = ["1", "2"; "3", "4"];
asserts.isequal(R, REF);
%=============================================================================
msg = sprintf(_('Conversion from cell failed. Element %d must be convertible to a string scalar.'), 1);
asserts.checkerror('R = string({[1, 2],2;3,4})', msg);
asserts.checkerror('R = string({[1, 2],2;3,4})', msg, 'Nelson:string:MustBeConvertibleCellArray');
%=============================================================================
R = string([1 2 NaN Inf]);
REF = ["1", "2", string(NaN), "Inf"];
asserts.isequal(R, REF);
%=============================================================================
R = string(inv(3));
REF = "0.33333";
asserts.isequal(R, REF);
%=============================================================================
R = string({''});
REF = "";
asserts.isequal(R, REF);
%=============================================================================
R = string('');
REF = "";
asserts.isequal(R, REF);
%=============================================================================
R = string(char(ones(0, 3)));
REF = string(ones(0, 1));
asserts.isequal(R, REF);
%=============================================================================
R = string(char(ones(3, 0)));
REF = [""; ""; ""];
asserts.isequal(R, REF);
%=============================================================================
R = string(char(ones(0, 0, 0, 0)))
REF = string(ones(0, 0, 0));
asserts.isequal(R, REF);
%=============================================================================
R = string(char(ones(3, 0, 2)))
REF = ["", ""; "", ""; "", ""];
asserts.isequal(R, REF);
%=============================================================================
