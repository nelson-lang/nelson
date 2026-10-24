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
asserts.isequal(R.char(), '{}');
%=============================================================================
R = pyrun("R = {'name': 'Dionysia', 'age': 28, 'location': 'Athens'}", "R");
asserts.checkerror('R.cell', sprintf(_('Invalid property: %s.'), 'cell'))
asserts.isequal(R.char(), '{''name'': ''Dionysia'', ''age'': 28, ''location'': ''Athens''}')
%=============================================================================
st = R.struct();
asserts.isequal(fieldnames(st), {'name'; 'age'; 'location'})
asserts.isequal(class(st.name), 'py.str');
asserts.isequal(st.name.char, 'Dionysia');
asserts.isequal(class(st.age), 'py.int');
asserts.isequal(st.age.double, 28);
asserts.isequal(class(st.location), 'py.str');
asserts.isequal(st.location.char, 'Athens');
%=============================================================================