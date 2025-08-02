%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INTERACTIVE TEST-->
% <--GUI MODE-->
%=============================================================================
assert_isequal(nargin('questdlg'), -1);
assert_isequal(nargout('questdlg'), 1);
%=============================================================================
r = questdlg('Hello')
r = questdlg('Hello', 'Franck')
r = questdlg('What is the answer to the ultimate question of life, the universe and everything ?', 'A question for geeks', '41', '42', '43', '42')
r = questdlg ('Easy ?', 'Jeff', 'No', 'Okay', 'Okay')
r = questdlg({'Is', 'this', 'a', 'multi line', 'test ?'}, 'Test :)')
r = questdlg('How are you ?', 'Health', 'Fine', 'Good', 'sick', 'Fine')
%=============================================================================
