%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
R = pyrun('R = {}', "R");
assert_isequal(R.char(), '{}');
%=============================================================================
R = pyrun("R = {'name': 'Dionysia', 'age': 28, 'location': 'Athens'}", "R");
assert_checkerror('R.cell', _('Wrong value for #2 argument. cell'))
assert_isequal(R.char(), '{''name'': ''Dionysia'', ''age'': 28, ''location'': ''Athens''}')
%=============================================================================
st = R.struct();
assert_isequal(fieldnames(st), {'name'; 'age'; 'location'})
assert_isequal(class(st.name), 'py.str');
assert_isequal(st.name.char, 'Dionysia');
assert_isequal(class(st.age), 'py.int');
assert_isequal(st.age.double, 28);
assert_isequal(class(st.location), 'py.str');
assert_isequal(st.location.char, 'Athens');
%=============================================================================