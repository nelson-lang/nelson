%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('string'), 1);
assert_isequal(nargout('string'), 1);
%=============================================================================
R = string({1,2;3,4});
REF = ["1", "2"; "3", "4"];
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = string({[1, 2],2;3,4})', _('Unable to convert supplied object to a string.'));
%=============================================================================
R = string([1 2 NaN Inf]);
REF = ["1", "2", string(NaN), "Inf"];
assert_isequal(R, REF);
%=============================================================================
R = string(inv(3));
REF = "0.3333";
assert_isequal(R, REF);
%=============================================================================
R = string({''});
REF = "";
assert_isequal(R, REF);
%=============================================================================
R = string('');
REF = "";
assert_isequal(R, REF);
%=============================================================================
R = string(char(ones(0, 3)));
REF = string(ones(0, 1));
assert_isequal(R, REF);
%=============================================================================
R = string(char(ones(3, 0)));
REF = ["";"";""];
assert_isequal(R, REF);
%=============================================================================
R = string(char(ones(0, 0,0,0)))
REF = string(ones(0, 0, 0));
assert_isequal(R, REF);
%=============================================================================
R = string(char(ones(3, 0,2)))
REF = ["", ""; "", ""; "", ""];
assert_isequal(R, REF);
%=============================================================================
