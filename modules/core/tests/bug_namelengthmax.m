%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ID = ['A', char(double('0') * ones(1, namelengthmax -1 ))];
assert_isequal(length(ID), namelengthmax);
STR = [ID, ' = 3'];
execstr(STR);
%=============================================================================
ID = ['A', char(double('0') * ones(1, namelengthmax))];
assert_isequal(length(ID), namelengthmax + 1);
STR = [ID, ' = 3'];
assert_checkerror(STR, _('Maximum name length exceeded.'));
%=============================================================================

